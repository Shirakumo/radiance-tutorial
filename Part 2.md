# Part 2
[Part 1](Part 1.md)

## Resources & Documentation
Here are links to relevant documentation and resource pages that will be useful to refer to for this tutorial.

* [Radiance](https://shirakumo.github.io/radiance)
* [Interface Definitions](https://github.com/Shirakumo/radiance/blob/master/standard-interfaces.lisp)
* [Clip](https://shinmera.github.io/clip)
* [lQuery](https://shinmera.github.io/lquery)
* [LASS](https://shinmera.github.io/LASS)

## A Short Roadmap
Like before, we'll see which points to address in this part of the tutorial.

* Allow editing existing pastes
* Allow deleting pastes

Ok, let's go.

## Editing Pastes
First, let's change up the `edit` page to accept an ID in its url and potentially use the loaded paste to fill in the template, similar to the `view` page.

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

At this point you might realise that we're duplicating quite a bit of functionality. Not to mention that the code looks a bit too elaborate for what we're doing. Let's try something else instead. 

## Cleaning up Data Access
We'll create some functions for the programmatical handling of our pastes. As part of that, we'll make use of another of Radiance's interface, namely the `data-model`. This provides a very thin but convenient wrapper around database records. Time to change and reload our system.

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
                            :path (format NIL "view/~a" (dm:id paste))))
        (api-output `(("id" . ,(dm:id paste)))))))
```

Not much has changed here aside from the access to the ID. For the view, we'll also update the template a bit, both to make things look a bit simpler in our page, and in order to display the time that we haven't done anything with before now. Here's the new page definition:

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
          <nav class="actions">
            <input type="submit" @formaction="/edit/{0} _id" value="Edit" />
          </nav>
        </form>
      </c:using>
    </main>
    <footer>
    </footer>
  </body>
</html>
```

Now we have something new going on! The `<c:using>` tag is kind of like a `let` binding and replaces the current set of variables -- called the "clipboard" -- with whatever value is resolved in its `value` attribute. Anything within the tag then resolves its variables using this new object, in our case the paste data-model. The actual tag will be removed upon compilation and its contents are just spliced in-place.

I've also added a `<time>` tag, which will be automatically filled with appropriate data by the lQuery `time` function. The actions have also been extended with a submit button that should take you to the appropriate page. Interesting here is that the `@formaction` attribute contains a URI with a pattern, and an argument. All of the `@` tags in Clip can take a URI and a number of arguments that are used to fill in the numbered placeholders in the URI.

With that said, let's return to our initial objective.

## Editing Pastes -- Take 2
With a proper edit button in place, and data handling cleaned up, let's give the edit page another try.

```common-lisp
(define-page edit "plaster/edit(/(.*))?" (:uri-groups (NIL id) :lquery "edit.ctml")
  (let ((paste (if id
                   (ensure-paste id)
                   (dm:hull 'plaster-pastes))))
    (r-clip:process T :paste paste)))
```

Alright, that's simple enough. Now we just need to fix up the edit template like we did for the view.

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
        <form class="edit" method="post" action="#">
          <header>
            <input type="text" name="title" placeholder="Untitled" maxlength="32" lquery="(val title)" />
          </header>
          <textarea name="text" placeholder="Paste something here" autofocus required lquery="(text text)"></textarea>
          <nav class="actions">
            <input type="hidden" name="id" lquery="(val _id)" />
            <input type="hidden" name="browser" value="true" />
            <c:if test="(dm:hull-p *)">
              <c:then>
                <input type="submit" @formaction="/api/plaster/new" value="Post" />
              </c:then>
              <c:else>
                <input type="submit" @formaction="/api/plaster/edit" value="Save" />
              </c:else>
            </c:if>
          </nav>
        </form>
      </c:using>
    </main>
    <footer>
    </footer>
  </body>
</html>
```

Here things become a bit more special. Aside from the added `lquery` tags to fill in the fields and the `<c:using>` tag like before, we now also have a `<c:if>`. This works pretty much exactly like you would imagine it to. It evaluates the code in its `test` attribute. If this evaluates to a non-`NIL` value, the `<c:then>` tag is evaluated and spliced in place of the `<c:if>`. Otherwise, the same is done but for the `<c:else>` tag. This, coupled with the `@formaction` allows us to use a different API endpoint depending on whether we're currently creating a new paste, or editing it. You might also be curious about the `*` within the `<c:if>` test. In Clip, the `*` is bound to the current clipboard object, which thanks to the `<c:using>` surrounding the `<c:if>` is the paste data-model instance.

Assuming that you've already tried the pasting out before, if you visit [the paste view page](https://localhost:8080/!/plaster/view/0) now, you'll be able to successfully click on the Edit button and be lead to the editing page. Actually editing the paste won't work quite yet, but it will fail somewhat gracefully. Hitting the "Edit" button will redirect you back to the same page. You might then realise that we've been redirected back with an `error` GET parameter added, though. Let's incorporate that into the page so that the user can actually see it in a useful way.

```common-lisp
(define-page edit "plaster/edit(/(.*))?" (:uri-groups (NIL id) :lquery "edit.ctml")
  (let ((paste (if id
                   (ensure-paste id)
                   (dm:hull 'plaster-pastes))))
    (r-clip:process T :paste paste
                      :error (get-var "error"))))
```

All that's changed is that there's another Clip variable for the error message that's taken from the get parameter. The change to the edit template is also not big. Just add the following snippet somewhere appropriate outside the `<c:using>` tag.

```HTML
<c:when test="error">
  <div class="infobox" id="error" lquery="(text error)">ERROR</div>
</c:when>
```

Similar to Lisp's `when` and `unless`, Clip also supports these shorthands to make the common cases of conditionals shorter. If you refresh your page after these changes, you should get a big 'ol error box explaining what's wrong.

Naturally we can't edit yet because the API endpoint doesn't exist. Easy enough to fix.

```common-lisp
(defun api-paste-output (paste)
  (if (string= "true" (post/get "browser"))
      (redirect (make-uri :domains '("plaster")
                          :path (format NIL "view/~a" (dm:id paste))))
      (api-output (loop for field in (dm:fields paste)
                        collect (cons field (dm:field paste field))))))

(define-api plaster/edit (id &optional text title) ()
  (let ((paste (ensure-paste id)))
    (when text (setf (dm:field paste "text") text))
    (when title (setf (dm:field paste "title") title))
    (dm:save paste)
    (api-paste-output paste)))
```

I've extracted the output/redirect behaviour into its own function here in order to eliminate the duplication from the two API endpoints. While I was at it, I've also made it return all of the data it has on a non-browser request, which should be a lot more useful.

The actual updating of the paste record in the edit endpoint is nothing surprising. The data-model wrapper will take care of most of it. All we have to do is retrieve it, set the fields, and save it. If you give the editing a whirl now, it should all work out splendidly.

## Deleting Pastes
Adding the ability to delete pastes from here is not going to be much of a challenge anymore. We'll just need a new API endpoint and another button on the editing template.

```common-lisp
(define-api plaster/delete (id) ()
  (let ((paste (ensure-paste id)))
    (dm:delete paste)
    (if (string= "true" (post/get "browser"))
        (redirect (uri-to-url "plaster/edit"
                              :representation :external
                              :query '(("message" . "Paste deleted"))))
        (api-output `(("_id" . ,(dm:id paste)))))))
```

But oh! The `redirect` changed here from what we've seen before. Instead of directly passing it a URI, we're instead transforming the URI manually into an URL with some extra arguments. This is done because URIs do not store the query part of a URL. However, since we want to give the user some kind of feedback after a successful deletion, we should add a message to the edit page. We can do this with a GET argument, which we need to mix into the URL by `uri-to-url`. The `:representation :external` makes sure that the URI is transformed into a URL that points to resources that are valid on the HTML page, rather than internally. Again, this is part of the routing system, which we'll look at more in-depth in a later part.

All that's left is changing the template and page. Adding the following snippet before the `Save` button in `edit.ctml` should take care of the button.

```HTML
<input type="submit" @formaction="/api/plaster/delete" value="Delete" />
```

For the message we need to do basically the same thing as for the error response.

```HTML
<c:when test="message">
  <div class="infobox" id="message" lquery="(text message)">MESSAGE</div>
</c:when>
```

And now to add the argument to the Clip processing call.

```common-lisp
(define-page edit "plaster/edit(/(.*))?" (:uri-groups (NIL id) :lquery "edit.ctml")
  (let ((paste (if id
                   (ensure-paste id)
                   (dm:hull 'plaster-pastes))))
    (r-clip:process T :paste paste
                      :error (get-var "error")
                      :message (get-var "message"))))
```

And we're all done already.

With that we've got a semi-sensible paste service up and running. In the next part we'll look at even more actions one might want to do with pastes. Those will however require a bit more serious restructuring than what we've been doing in this part.

[Part 3](Part 3.md)
