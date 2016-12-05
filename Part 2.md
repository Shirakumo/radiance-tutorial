# Part 2
## A Short Roadmap
Like before, we'll see which points to address in this part of the tutorial.

* Allow editing existing pastes
* Allow deleting pastes
* Check the validity of arguments

Ok, let's go.

## Editing Pastes
First, let's change up the `edit` page to accept an ID in its url and potentially use the loaded paste to fill in the template, similar to the `view` page.

```common-lisp

```

## Verification and Correctness
An important, but often forgotten aspect is the validation of data that an API receives. So far we don't really have much to validate, but nevertheless it is important to keep it in mind. Let's augment our API endpoints for some checks.

```common-lisp
(define-page edit "plaster/edit(/(.*))?" (:uri-groups (NIL id) :lquery "edit.ctml")
  (if id
      (let ((id (parse-integer id))
            (paste (first (db:select 'plaster-pastes (db:query (:= '_id id)) :amount 1))))
        (unless paste
          (error 'request-not-found :message (format NIL "No paste with ID ~a was found." id)))
        (r-clip:process
         T
         :title (gethash "title" paste)
         :time (gethash "time" paste)
         :text (gethash "text" paste)))
      (r-clip:process T)))
```

At this point you might realise that we're duplicating quite a bit of functionality. Not to mention that the code looks a bit too elaborate for what we're doing. Let's try something else instead. We'll create some functions for the programmatical handling of our pastes. As part of that, we'll make use of another of Radiance's interface, namely the `data-model`. This provides a very thin but convenient wrapper around database records. Time to change and reload our system.

```common-lisp
(asdf:defsystem #:plaster
  ...
  :depends-on ((:interface :database)
               (:interface :data-model)
               :r-clip))
```

Now, let's write some convenience functions.

```common-lisp
(defun ensure-paste (paste-ish)
  (etypecase paste-ish
    (dm:data-model paste-ish)
    (string (ensure-paste (parse-integer paste-ish)))
    (integer (or (dm:get-one 'plaster-pastes (db:query (:= '_id paste-ish)))
                 (error 'request-not-found :message (format NIL "No paste with ID ~a was found." paste-ish))))))

(defun create-paste (text &key title)
  (let ((paste (dm:hull 'plaster-pastes)))
    (setf (dm:field paste "text") text
          (dm:field paste "title") (or title "")
          (dm:field paste "time") (get-universal-time))
    (dm:insert paste)))
```

The data-model interface gives us a convenient function to fetch a single record and something a bit less ad-hoc to insert a new record too. An actual data-model instance is nothing more than a map of fields to values and the name of the collection that it belongs to. It then supports the operations `dm:insert`, `dm:delete`, and `dm:save` to manage the persistence of the model in the database.

Now that we have these convenience functions we should update our API endpoint and our `view` page to use it.

```common-lisp
(define-api plaster/new (text &optional title) ()
  (let ((paste (create-paste text :title title)))
    (if (string= "true" (post/get "browser"))
        (redirect (make-uri :domains '("plaster")
                            :path (format NIL "view/~a" id)))
        (api-output `(("id" . ,(dm:id paste)))))))
```

Not much has changed here. For the view, we'll also update the template a bit, both to make things look a bit simpler in our page, and in order to display the time that we haven't done anything with before now. Here's the new page definition:

```common-lisp
(define-page view "plaster/view/(.*)" (:uri-groups (id) :lquery "view.ctml")
  (r-clip:process T :paste (ensure-paste id)))
```

Ah! Much simpler. The template now only gets a single variable, namely our paste object. Luckily enough, Clip makes it convenient to extract the information anyway:

```HTML
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta charset="utf-8" />
    <title>Plaster</title>
    <link rel="stylesheet" type="text/css" href="../static/plaster.css" @href="/static/plaster/plaster.css" />
  </head>
  <body>
    <header>
      <h1>Plaster</h1>
      <nav>
        <a href="#" @href="plaster/edit">New</a>
      </nav>
    </header>
    <main>
      <c:using value="paste">
        <form class="edit">
          <header>
            <input type="text" name="title" placeholder="Untitled" maxlength="32" readonly
                   lquery="(val title)" />
            <time lquery="(time time)">2106.10.23</time>
          </header>
          <textarea name="text" placeholder="Paste something here" readonly
                    lquery="(text text)"></textarea>
          <nav class="actions"></nav>
        </form>
      </c:using>
    </main>
    <footer>
    </footer>
  </body>
</html>
```

Now we have something new going on! The `<c:using>` tag is kind of like a `let` binding and replaces the current set of variables -- called the "clipboard" -- with whatever value is resolved in its `value` attribute. Anything within the tag then resolves its variables using this new object, in our case the paste data-model. The actual tag will be removed upon compilation and its contents are just spliced in-place.

I've also added a `<time>` tag, which will be automatically filled with appropriate data by the lQuery `time` function.
