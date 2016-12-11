# Part 3
[Part 0](Part 0.md) [Part 1](Part 1.md) [Part 2](Part 2.md)

## Resources & Documentation
Here are links to relevant documentation and resource pages that will be useful to refer to for this tutorial.

* [Radiance](https://shirakumo.github.io/radiance)
* [Interface Definitions](https://github.com/Shirakumo/radiance/blob/master/standard-interfaces.lisp)
* [Clip](https://shinmera.github.io/clip)
* [lQuery](https://shinmera.github.io/lquery)
* [LASS](https://shinmera.github.io/LASS)
* [Crypto-Shortcuts](https://shinmera.github.io/crypto-shortcuts)
* [CodeMirror](http://codemirror.net/doc/manual.html)
* [Ubiquitous](https://shinmera.github.io/ubiquitous)

## A Short Roadmap
On the menu for today we have...

* Raw paste view
* Repasting
* Annotating pastes

Order up!

## Raw Paste View
This one is very easy. We'll add a new page that just retrieves the paste and returns its text field.

```common-lisp
(define-page raw "plaster/view/(.*)/raw" (:uri-groups (id))
  (setf (content-type *response*) "text/plain")
  (dm:field (ensure-paste id) "text"))
```

And done. It is important not to forget to set the content-type of the response to `text/plain`, lest the paste service could be abused to host HTML pages. Note also that despite us defining a new page that has the same URI prefix as the `view` page, the dispatching will still work just fine. This is due to Radiance's URI precedence ordering, which you can read up on in detail [in the documentation](https://shirakumo.github.io/radiance/#RADIANCE-CORE:URI-DISPATCHER>).

Should you ever encounter a situation where the automatic ordering is not doing what you need, you can instead pass an explicit priority number to your page definition like so:

```common-lisp
(define-page (raw 1000) ...)
```

Finally we also want a lil' link to the raw view of the pastes, and we can achieve this with another `<a>` in the actions nav of our view template.

```HTML
<a href="#" @href="plaster/view/{0}/raw _id">Raw</a>
```

## Repasting
Repasting is useful when you want to create a new paste based on an old one. It's especially useful when you don't have permission to edit an old paste, but that's for later.

Thinking about it, all the repasting needs to do is pre-fill the fields of the edit page with the information of a previous paste and show the `Post` button instead of the `Save` one. We can solve this neatly by changing our `<c:if>` test slightly and adding a new GET argument.

```common-lisp
(define-page edit "plaster/edit(/(.*))?" (:uri-groups (NIL id) :lquery "edit.ctml")
  (let ((paste (if id
                   (ensure-paste id)
                   (dm:hull 'plaster-pastes))))
    (r-clip:process T :paste paste
                      :repaste (get-var "repaste")
                      :error (get-var "error")
                      :message (get-var "message"))))
```

The new `test` attribute could look like this:

```common-lisp
(or repaste (dm:hull-p *))
```

But hold on for a second. This is within the `<c:using>` tag, so we can't access the `repaste` field because that's not part of the paste data-model object, but rather the context outside of it. Thankfully, Clip allows you to retrieve fields from surrounding clipboards as well.

```common-lisp
(or (** :repaste) (dm:hull-p *))
```

Clip simulates an arbitrary number of "asterisk functions" that allow you to go further and further outside of the current stack of clipboards. Each asterisk is a level further up the stack. Thus, you never have to worry about accidentally shadowing your variables in a Clip template.

A short button addition to the view template later and we're already all done.

```HTML
<a href="#" @href="plaster/edit/{0}?repaste _id">Repaste</a>
```

## Annotating
Annotations are pastes that are a child of a parent paste. This is a very useful feature for correlating pastes and collaborating with people. It allows you to show iterations and evolutions of a paste, or post separate files that belong together in one page. However, implementing them is going to require a bit more effort than repasting.

Let's add a new collection to our database to describe the relationship between pastes and their annotations.

```common-lisp
(db:create 'plaster-annotations '((paste :id)
                                  (annotation :id)))
```

Let's also make some convenience functions to query that part of our data.

```common-lisp
(defun paste-annotations (paste)
  (let* ((paste (ensure-paste paste))
         (rels (dm:get 'plaster-annotations (db:query (:= 'paste (dm:id paste)))
                       :sort '((time :ASC)))))
    (loop for rel in rels
          collect (ensure-paste (dm:field rel "annotation")))))

(defun paste-parent (paste)
  (let* ((paste (ensure-paste paste))
         (rel (dm:get-one 'plaster-annotations (db:query (:= 'annotation (dm:id paste))))))
    (when rel
      (ensure-paste (dm:field rel "paste")))))
```

Next we just need to update our `create-paste` function to take a potential parent ID, which then automatically makes the paste an annotation as well.

```common-lisp
(defun create-paste (text &key title parent)
  (db:with-transaction ()
    (let ((paste (dm:hull 'plaster-pastes))
          (parent (when parent (ensure-paste parent))))
      (setf (dm:field paste "text") text
            (dm:field paste "title") (or title "")
            (dm:field paste "time") (get-universal-time))
      (dm:insert paste)
      (when parent
        (when (paste-parent parent)
          (error "Cannot annotate an annotation."))
        (db:insert 'plaster-annotations
                   `(("paste" . ,(dm:id parent))
                     ("annotation" . ,(dm:id paste)))))
      paste)))
```

Some things to note here. The bulk of the body has been wrapped in a `db:with-transaction`, which promises to roll back any changes if an unexpected unwind or conflict happens. Then we simply add an insert statement that puts the correlation between the new paste and its parents into that collection. We `ensure-paste` in order to ascertain that the parent actually exists, and that we can potentially also supply the `parent` argument with different types, rather than constraining it to a direct ID. We also check if the parent is registered as an annotation and error in that case, since we do not want to allow deep nesting. Finally we just ensure that the proper paste object is returned from the function.

Now that creation is taking care of, we also need to handle deletion. If a parent gets removed, all of its annotations should probably also get taken care of. Let's create a dedicated `delete-paste` function that we can use instead of the ad-hoc call we've had before to wrap that up.

```common-lisp
(defun delete-paste (paste)
  (db:with-transaction ()
    (let ((paste (ensure-paste paste)))
      (mapc #'dm:delete (paste-annotations paste))
      (db:remove 'plaster-annotations
                 (db:query (:or (:= 'paste (dm:id paste))
                                (:= 'annotation (dm:id paste)))))
      (dm:delete paste)
      paste)))

(define-api plaster/delete (id &optional current-password) ()
  (let ((paste (ensure-paste id)))
    (delete-paste paste)
    (if (string= "true" (post/get "browser"))
        (redirect (uri-to-url "plaster/edit"
                              :representation :external
                              :query '(("message" . "Paste deleted"))))
        (api-output `(("_id" . ,(dm:id paste)))))))
```

Again we stuff everything into a transaction to ensure consistency. After all, it might happen that someone annotates right while we delete the parent paste, in which case things might go awry. With the transaction in place, either one of them blocks before the other can go, or one of them always fails. Either way, our model stays sound. As illustrated, the `db:query` selector also supports logical combination of tests which we can use to drop all relevant records at once, rather than having to go through twice.

Next we'll also want to update our `api-paste-output` function to redirect to the parent paste in case our paste is an annotation.

```common-lisp
(defun api-paste-output (paste)
  (cond ((string= "true" (post/get "browser"))
         (let ((parent (paste-parent paste)))
           (redirect (make-url :domains '("plaster")
                               :path (format NIL "view/~a"
                                             (if parent
                                                 (dm:id parent)
                                                 (dm:id paste)))
                               :fragment (princ-to-string (dm:id paste))))))
        (T
         (api-output (loop for field in (dm:fields paste)
                           collect (cons field (dm:field paste field)))))))
```

Here we're using the `make-url` shorthand function to directly construct a URI, translate it, and add t he missing URL parts in one go. With that all set, all we need to do now is update our `new` endpoint to take a parent ID as an optional argument, our `view` page to pass the annotations to the template, and our `edit` page to handle the case of wanting to annotate.

```common-lisp
(define-api plaster/new (text &optional title parent) ()
  (let ((paste (create-paste text :title title :parent parent)))
    (api-paste-output paste)))

(define-page view "plaster/view/(.*)" (:uri-groups (id) :lquery "view.ctml")
  (let* ((paste (ensure-paste id))
         (annotations (sort (paste-annotations paste)
                            #'< :key (lambda (a) (dm:field a "time")))))
    (r-clip:process T :paste paste :annotations annotations)))

(define-page edit "plaster/edit(/(.*))?" (:uri-groups (NIL id) :lquery "edit.ctml")
  (let ((paste (if id
                   (ensure-paste id)
                   (dm:hull 'plaster-pastes))))
    (r-clip:process T :paste paste
                      :parent (get-var "annotate")
                      :repaste (get-var "repaste")
                      :error (get-var "error")
                      :message (get-var "message"))))
```

In the view we must make sure to sort the annotations by their timestamp. Otherwise, they might end up in a random order from the database, and I'm sure that isn't exactly desirable behaviour.

We chose the GET variable name "annotate" for the `edit` page because it looks and sounds a bit more intuitive to use from a user perspective. Internally however, we use `parent` for everything as it describes the relationship a bit more usefully.

Ok! With the code side of things settled first this time around, we'll need to go back and update our templates. Let's start with the boring one, namely the edit template. We just need an additional, hidden field for this, which we can stuff alongside the other elements in the actions nav.

```HTML
<input type="hidden" name="parent" lquery="(val (** :parent))" />
```

As before, we use the double-asterisks pseudo function to escape the context of the `<c:using>`. That should be all for `edit.ctml`. On to `view.ctml`.

First, another button to our actions nav:

```HTML
<a href="#" @href="plaster/edit?annotate={0} _id">Annotate</a>
```

And second, a new section below our `<c:using>` tag that will be used to display all of the annotations. This one's a bit bigger than our HTML snippets have been for a while.

```HTML
<c:when test="annotations">
  <section id="annotations">
    <h2>Annotations</h2>
    <ul iterate="annotations">
      <li>
        <form class="edit" lquery="(attr :id _id)">
          <header>
            <input type="text" name="title" placeholder="Untitled" maxlength="32" readonly
                   lquery="(val title)" />
            <time lquery="(time time)">2106.10.23</time>
          </header>
          <textarea name="text" placeholder="Paste something here" readonly
                    lquery="(text text)"></textarea>
          <nav class="actions">
            <a href="#" @href="plaster/view/{0} _id">Raw</a>
            <a href="#" @href="plaster/edit/{0}?repaste _id">Repaste</a>
            <a href="#" @href="plaster/edit/{0} _id">Edit</a>
          </nav>
        </form>
      </li>
    </ul>
  </section>
</c:when>
```

For the most part however, it is just a copy of the actual paste form, with the annotation button removed. The rest is just an `<ul><li>` tagging around it, and a nice section for readability. The interesting part here is the Clip `iterate` attribute, which will iterate over its value, copy the contents of its element every time and put the evaluated result of that copy into itself. More simply put, it probably does about what you would expect it to do.

Finally we need to update our stylesheet to account for the new annotations section. Add the following snippet within the `>main` block and you should be all set.

```common-lisp
("#annotations"
 :margin 0 10px 0 10px
 (h2
  :color (rgb 255 255 255)
  :border-bottom 3px solid (rgb 0 130 240))
 (ul
  :list-style none
  :padding 0
  (.edit
   :margin 0 0 5px 0
   (header
    :padding 5px
    :font-size 0.8em
    :border-width 1px)
   (textarea
    :min-height 100px))))
```

And that's all she wrote. You can now try out annotating. If you did it all right, it should work marvellously. If not, retrace your steps, compare the code, and I'm sure you'll be able to find a mistake soon enough.

[Part 4](Part 4.md)
