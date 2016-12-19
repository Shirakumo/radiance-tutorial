# Part 4
[Part 0](Part 0.md) [Part 1](Part 1.md) [Part 2](Part 2.md) [Part 3](Part 3.md)

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
Well then. Our homework for today is:

* A public list of pastes
* Being able to unlist and protect pastes

Let's get crackin'.

## A Public Paste List
We're finally at the point where we need to split off from our view and edit pages, and create a new one. So fire up your editor because it's time to write some more HTML.

For the most part the page should be rather simple-- just a listing of some number of recent pastes plus a page navigation to go back and forth in time. We will call this `list.ctml`.

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
      <section class="paste-list public">
        <header>
          <h2>Public Paste List</h2>
        </header>
        <c:when test="(< 0 page)">
          <a class="button newer" href="#" @href="plaster/list/{0} (1- page)">Newer</a>
        </c:when>
        <ul iterate="pastes">
          <li>
            <a href="#" @href="plaster/view/{0} _id">
              <span class="id" lquery="(text _id)">ID</span>
              <span class="title" lquery="(text title)">TITLE</span>
              <time lquery="(time time)" />
            </a>
          </li>
        </ul>
        <c:when test="has-more">
          <a class="button older" href="#" @href="plaster/list/{0} (1+ page)">Older</a>
        </c:when>
      </section>
    </main>
  </body>
</html>
```

There's nothing really new in this template that I haven't explained before. We create a `<ul>` that iterates over all pastes and outputs a nice `<li>` for each of them. Once again you can view it in your browser before we create a page to style it out and see how it'll look. Speaking of styling, here's some more stuff for the stylesheet if you don't want to do it yourself.

```common-lisp
(.button
 :display block)
(.paste-list
 (ul
  :background (rgb 255 255 255)
  :list-style none
  :padding 0
  :margin 0
  (li (a :background none
         :display flex
         :color (rgb 0 0 0)
         :font-weight normal
         (.id :min-width 50px)
         (.title :flex-grow 1))
      ("a:hover"
       :background (rgb 220 220 220)))))
```

The actual page to present this isn't too complicated either:

```common-lisp
(defparameter *pastes-per-page* 25)

(define-page list "plaster/list(?:/(.*))?" (:uri-groups (page) :clip "list.ctml")
  (let* ((page (or (when page (parse-integer page :junk-allowed T)) 0))
         (pastes (dm:get 'plaster-pastes (db:query :all)
                         :sort '((time :DESC))
                         :skip (* page *pastes-per-page*)
                         :amount *pastes-per-page*)))
    (r-clip:process T :pastes pastes
                      :page page
                      :has-more (<= *pastes-per-page* (length pastes)))))
```

To get the pagination going we use the `:skip` and `:amount` arguments. The `:sort` clause ensures deterministic ordering of the results. Finally we use a dynamic variable to hold how many pastes we want to display on each page and supply the template with the necessary stuff. The special keyword `:all` in the `db:query` form produces a selector that simply lets through any record.

At this point I think it is worth to mention that a lot of the precise detail decisions of how things are done here are just my own whims, and it certainly could be done very differently too. For example, you could also output a `:has-less` argument to the template instead of manually testing for `(< 0 page)`. You could display the current page number somewhere. You could rearrange the templates, store the data differently, etc. I'm sure you'll find a way to do things exactly the way you want to do them once you're moving on to your own project. For now, simply humour me and see this more as a display of a possible way to get things done, rather than "the correct way".

Know also that, while this tutorial does follow a rather incremental prototyping fashion of getting things done, this is not representative of how I went about things while writing the tutorial and application myself. I've been going back every now and again and retroactively changed and updated things as I learned about previous mistakes. This is a perfectly good way to go about development and Radiance / Common Lisp encourages this a lot, but it would only hurt the tutorial if I had to go back and forth all the time to explain changes all over. What you should take away from this is that you should try things out and experiment around. The interactive nature of everything allows you to rapidly go through prototypes and tests. This is an immensely powerful feature and you should make use of it.

Finally we should probably add a button to reach the list page to the other page's header section. The following snippet should do:

```HTML
<a href="#" @href="plaster/list">List</a>
```

Now, with the public paste list all done, let's move on to our second task.

## Unlisting and Protecting Pastes
Another frequent feature of paste services is allowing pastes to, in the very least, be unlisted from the public view, or to be protected by a password. Especially the password requires a bit more thought, as we need to have an intermittent view that prompts for a password before allowing you to actually see the paste.

But before we get to all that, let's for a second think about the changes necessary overall.

* The editor needs a way to select whether a paste is Public, Unlisted, or Private
* The editor needs a password field if the password is private
* The paste schema needs a new field for the visibility of the paste
* The listing needs to filter out unlisted and private pastes
* The viewer needs a password prompt wall to prevent unauthorised viewing

Ok, so that's quite a few changes that we need to make. I suppose we can get started with the templating, since that requires the least amount of concrete thought. I've added the following to the form's header section in `edit.ctml`.

```HTML
<div class="visibility">
  Visibility: <select name="visibility">
  <option value="1" lquery="(attr :selected (eql 1 visibility))">public</option>
  <option value="2" lquery="(attr :selected (eql 2 visibility))">unlisted</option>
  <option value="3" lquery="(attr :selected (eql 3 visibility))">private</option>
  </select>
  <input type="password" name="password" placeholder="password" />
</div>
```

In order to support the necessary field we need to change our schema slightly. Adding the following to our `db:create` should suffice:

```common-lisp
(visibility (:integer 1))
(password (:varchar 128))
```

This creates a signed integer field named `visibility` with the minimal size of one byte. Given that we only have three distinct visibilities, that should be fine. For the password field we're using 128 characters to represent a PBKDF2 hash. Naturally that's not the most efficient way of storing that information, but it should be fine for this.

First, some more functions to coerce some data.

```common-lisp
(defun ensure-visibility (vis-ish)
  (etypecase (or* vis-ish)
    (null 1)
    (integer (check-type vis-ish (integer 1 3)) vis-ish)
    (string (cond ((string-equal vis-ish "public") 1)
                  ((string-equal vis-ish "unlisted") 2)
                  ((string-equal vis-ish "private") 3)
                  (T (ensure-visibility (parse-integer vis-ish)))))))

(defparameter *password-salt* "Something ˢᵉᶜʳᵉᵗ")

(defun ensure-password (visibility password)
  (let ((password (or* password))
        (visibility (ensure-visibility visibility)))
    (when (and (= visibility 3) (not password))
      (api-error "A password is required for private visibility."))
    (when (and (/= visibility 3) password)
      (api-error "Cannot set a password on public or unlisted visibility.")))
  (when password (cryptos:pbkdf2-hash password *password-salt*)))
```

We allow both string and integer arguments for the visibility here in order to make the API a bit more "human". For the hashing of the password we've employed the help of another library called [Crypto-Shortcuts](https://shinmera.github.io/crypto-shortcuts), which should also be added to the ASDF system. This is also the first occurrence of the `or*` macro, which is just the same as `or`, but treats an empty string as NIL, which is a very useful behaviour for GET and POST arguments on web interfaces. The `api-error` is a shorthand for signalling a condition of type `api-error`, which will give useful API output rather than a generic failure page.

Next is the actual create function itself. While I was at it I've also factored out the annotation association.

```common-lisp
(defun register-annotation (annotation paste)
  (when (paste-parent paste)
    (error "Cannot annotate an annotation."))
  (db:insert 'plaster-annotations
             `(("paste" . ,(dm:id (ensure-paste paste)))
               ("annotation" . ,(dm:id (ensure-paste annotation))))))

(defun create-paste (text &key title parent visibility password)
  (db:with-transaction ()
    (let* ((paste (dm:hull 'plaster-pastes))
           (parent (when parent (ensure-paste parent)))
           (visibility (ensure-visibility visibility))
           (password (ensure-password visibility password)))
      (setf (dm:field paste "text") text
            (dm:field paste "title") (or title "")
            (dm:field paste "time") (get-universal-time)
            (dm:field paste "visibility") visibility
            (dm:field paste "password") password)
      (dm:insert paste)
      (when parent (register-annotation paste parent))
      paste)))
```

We first coerce the visibility to get a proper numerical value for it and hash the password if it is supplied. We then check if a password was supplied at all for the private visibility to ensure that you can't set it to private with no password at all. You could enforce even stricter password rules here if you so choose, but I have little interest for that. Then we just add the fields to the paste object like usual.

The editing process also needs some changes in case the visibility or password is changed. We also don't have a separated out editing function yet. Let's fix that while we're at it.

```common-lisp
(defun edit-paste (paste &key text title visibility password)
  (let* ((paste (ensure-paste paste)))
    (when text
      (setf (dm:field paste "text") text))
    (when title
      (setf (dm:field paste "title") title))
    (when visibility
      (setf (dm:field paste "visibility") (ensure-visibility visibility)))
    (when password
      (setf (dm:field paste "password") (ensure-password (or visibility (dm:field paste "visibility"))
                                                         password)))
    (dm:save paste)))
```

Not much of excitement to see here. For the password update we use the previous visibility value if no visibility change occurs. The edit endpoint looks boringly similar to the other endpoints now. Oh, and we should change the new endpoint too, of course.

```common-lisp
(define-api plaster/new (text &optional title parent visibility password) ()
  (let ((paste (create-paste text :title title :parent parent :visibility visibility :password password)))
    (api-paste-output paste)))

(define-api plaster/edit (id &optional text title visibility password) ()
  (let ((paste (edit-paste id :text text :title title :visibility visibility :password password)))
    (api-paste-output paste)))
```

You should now be able to paste with the different visibilities being remembered and checked properly. However, the actual behaviour on the list page and the view page are not yet implemented. The list page is fixed easily. All we need to do is change the query of the `dm:get` call from `(db:query :all)` to `(db:query (:= 'visibility 1))`.

When thinking about the view page, you might realise a bit of a pickle. Currently it's possible to also view annotations on their own, and they even have their own visibility. However, it is probably much more sensible for an annotation to inherit the visibility from its parent. The solution to this that I've opted for is to make the view page for an annotation redirect to the view page for its parent. The parent page can then handle the "authentication". At the same time, when creating an annotation the visibility selector and password field are hidden from the user, and the default visibility is set to unlisted. This'll require changes in a few places, but don't worry, it's not too big of an issue.

First, the template. Simply wrapping the visibility div in an `<c:unless test="(** :parent)">` element should be sufficient, but showing the user that they're annotating would probably be nice too, so let's do that instead. 

```HTML
<c:if test="(** :parent)">
  <c:then>
    <div class="annotate">
      Annotating paste <span lquery="(text (** :parent))">#</span>
    </div>
  </c:then>
  <c:else>
    <div class="visibility">
      Visibility: <select name="visibility">
      <option value="1" lquery="(attr :selected (eql 1 visibility))">public</option>
      <option value="2" lquery="(attr :selected (eql 2 visibility))">unlisted</option>
      <option value="3" lquery="(attr :selected (eql 3 visibility))">private</option>
      </select>
      <input type="password" name="password" placeholder="password" />
    </div>
  </c:else>
</c:if>
```

This fixes almost everything. However, this only handles the case where we create a new annotation. On the edit page of an annotation, the `annotate` GET parameter is not set and the visibility options will appear again. Not what we want. A slight modification to the template argument should fix this though.

```common-lisp
:parent (if id
            (let ((parent (paste-parent paste)))
              (when parent (dm:id parent)))
            (get-var "annotate"))
```

Not the prettiest way to do it, but it'll be fine for now.

Next the `create-paste` and `edit-paste` functions need to be made aware of the special casing of annotations. For the former, we'll first want to perform a check to see if the visibility was passed explicitly when a parent was passed as well and error in that case. Otherwise we default to a visibility of `2` if a parent was passed.

```common-lisp
(defun create-paste (text &key title parent visibility password)
  (when (and parent visibility)
    (api-error "Cannot set the visibility of an annotation."))
  (db:with-transaction ()
    (let* ((paste (dm:hull 'plaster-pastes))
           (parent (when parent (ensure-paste parent)))
           (visibility (if parent 2 (ensure-visibility visibility)))
           (password (ensure-password visibility password)))
      (setf (dm:field paste "text") text
            (dm:field paste "title") (or title "")
            (dm:field paste "time") (get-universal-time)
            (dm:field paste "visibility") visibility
            (dm:field paste "password") password)
      (dm:insert paste)
      (when parent (register-annotation paste parent))
      paste)))
```

We perform the same check in the `edit-paste` function.

```common-lisp
(defun edit-paste (paste &key text title visibility password)
  (db:with-transaction ()
    (let* ((paste (ensure-paste paste)))
      (when text
        (setf (dm:field paste "text") text))
      (when title
        (setf (dm:field paste "title") title))
      (when (and (paste-parent paste) visibility)
        (api-error "Cannot set the visibility of an annotation."))
      (when visibility
        (setf (dm:field paste "visibility") (ensure-visibility visibility)))
      (when password
        (setf (dm:field paste "password") (ensure-password (or visibility (dm:field paste "visibility"))
                                                           password)))
      (dm:save paste))))
```

Alright. With the checks in place, the last thing we have to do now is to modify the view page for two things. First, to redirect in the case of an annotation, and second to present a password prompt if no valid password was given to a private paste.

We've kind of already got the redirect logic properly encapsulated in another function called `api-paste-output`. Let's strip that out and put it into a function we can use directly instead.

```common-lisp
(defun paste-url (paste &optional (parent (paste-parent paste)))
  (let ((paste (ensure-paste paste)))
    (make-url :domains '("plaster")
              :path (format NIL "view/~a"
                            (if parent
                                (dm:id parent)
                                (dm:id paste)))
              :fragment (princ-to-string (dm:id paste)))))

(defun api-paste-output (paste)
  (cond ((string= "true" (post/get "browser"))
         (redirect (paste-url paste)))
        (T
         (api-output (loop for field in (dm:fields paste)
                           collect (cons field (dm:field paste field)))))))
```

I've made a very minor optimisation here where I allow the passing of the parent as an argument. In order to check whether the paste is an annotation we immediately retrieve the parent object on the view page already, so fetching it twice would be kind of stupid. Mind you, we've already got plenty of unnecessary database accesses and slight inefficiencies in the code, but I will gladly give those for the sake of clarity and brevity. In this case however, we don't lose much.

```common-lisp
(define-page view "plaster/view/(.*)" (:uri-groups (id) :clip "view.ctml")
  (let* ((paste (ensure-paste id))
         (parent (paste-parent paste)))
    (if parent
        (redirect (paste-url paste parent))
        (r-clip:process T :paste paste
                          :annotations (sort (paste-annotations paste)
                                             #'< :key (lambda (a) (dm:field a "time")))))))
```

With that all set and done, viewing an annotation directly should now properly redirect to its parent. On to the password logic.

A sensible way to go about all of this would be to show a password prompt page if no password was given, the usual paste page on a correct password, and a permission denied error page on an incorrect one. Shouldn't be too hard, we just need to add some more clauses to that `if` of ours.

```common-lisp
(define-page view "plaster/view/(.*)" (:uri-groups (id) :clip "view.ctml")
  (let* ((paste (ensure-paste id))
         (parent (paste-parent paste))
         (password (post/get "password")))
    (cond (parent
           (redirect (paste-url paste parent)))
          ((or (/= 3 (dm:field paste "visibility"))
               (string= (dm:field paste "password") (cryptos:pbkdf2-hash password *password-salt*)))
           (r-clip:process T :paste paste
                             :annotations (sort (paste-annotations paste)
                                                #'< :key (lambda (a) (dm:field a "time")))))
          ((or* password)
           (error 'request-denied :message "The supplied password is incorrect."))
          (T
           (r-clip:process (@template "password.ctml"))))))
```

The `post/get` function is another useful one that allows you to retrieve a parameter that can both be a POST or a GET parameter. We allow both as it might be useful for someone to distribute a private paste with the password in its URL directly to others. Otherwise a POST parameter would be preferred for the exact opposite reason.

We then check if the paste is a "normal" one, or if the password matches perfectly. If so we proceed as usual. Otherwise, if the password was supplied (but was proven to be wrong), then an error is signalled. Finally, if no password was supplied at all we render a different template. The `@template` macro here constructs a path relative to the current module's template directory.

Alright, now we need to create that password template.

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
        <a href="#" @href="plaster/list">List</a>
      </nav>
    </header>
    <main>
      <form class="password-prompt" method="post">
        <header>
          <h2>A Password Is Required for This Paste</h2>
        </header>
        <input type="password" name="password" placeholder="password" autofocus required />
        <input type="submit" value="Unlock" />
      </form>
    </main>
  </body>
</html>
```

Nothing big or unusual to see here. As discussed above we use the POST method to hide the password from the URL by default, which should be the saner option. Go ahead and try it all out too, the password protection should work just fine.

Great, so we're all done, finally.

Or are we? Unfortunately we're not. Think about what the password protection currently does! All it's useful for is protecting the view page, but there are other ways to gain access to the paste too! You can repaste or edit a paste too, and those will happily show you to the contents of the paste without any prompts for a password. The API endpoints aren't properly secured either-- for example, you probably shouldn't be able to edit or annotate a paste you don't have a password for.

Before we get too far into a rush about security, let's take things one at a time.

First, in order to protect the edit page in the same way as the view page, let's extract the password logic into a `call-with` scheme.

```common-lisp
(defun call-with-password-protection (function paste &optional (password (post/get "password")))
  (cond ((or (not (eql 3 (dm:field paste "visibility")))
             (string= (dm:field paste "password") (cryptos:pbkdf2-hash password *password-salt*)))
         (funcall function))
        ((or* password)
         (error 'request-denied :message "The supplied password is incorrect."))
        (T
         (r-clip:process (@template "password.ctml")))))

(defmacro with-password-protection ((paste &optional (password '(post/get "password"))) &body body)
  `(call-with-password-protection
    (lambda () ,@body) ,paste ,password))
```

Attentive readers may notice that I've changed the `/=` to a `not eql` here. This is in preparation for the use in the edit page, where the paste can be a hull, in which case the visibility field is `NIL` and thus not a number. With that out of the way, here are the modified view and edit pages:

```common-lisp
(define-page view "plaster/view/(.*)" (:uri-groups (id) :clip "view.ctml")
  (let* ((paste (ensure-paste id))
         (parent (paste-parent paste)))
    (if parent
        (redirect (paste-url paste parent))
        (with-password-protection (paste)
          (r-clip:process T :paste paste
                            :annotations (sort (paste-annotations paste)
                                               #'< :key (lambda (a) (dm:field a "time"))))))))

(define-page edit "plaster/edit(?:/(.*))?" (:uri-groups (id) :clip "edit.ctml")
  (let* ((paste (if id
                    (ensure-paste id)
                    (dm:hull 'plaster-pastes)))
         (parent (if id
                     (paste-parent paste)
                     (when (get-var "annotate") (ensure-paste (get-var "annotate"))))))
    (with-password-protection ((or parent paste))
      (r-clip:process T :paste paste
                        :parent (when parent (dm:id parent))
                        :repaste (get-var "repaste")
                        :error (get-var "error")
                        :message (get-var "message")))))
```

Note the careful change in the edit page. We have to password protect against the parent or requested annotation target if one exists, since we should inherit the password in the case of an annotation. Without this, one could edit or create annotations of a password protected paste without needing to supply the password. Nasty!

While we're dealing with the user interface, another issue pops up though. Namely, currently the user has to type in the password again when getting to the edit page from theview page. Plus, the edit page does not prefill the previous password. This is all rather cumbersome, but we can fix it easily enough in the templates. They just need a new argument `:password` that is filled with the POST/GET variable from the view/edit pages.

In the view template all the `@href`s need to be changed to supply the password in the query part. For the sake of brevity, I'll only paste one example of each here:

```HTML
<a href="#" @href="plaster/edit?annotate={0}&password={1} _id (** :password)">Annotate</a>
```

The edit template needs its password field changed slightly, too.

```HTML
<input type="password" name="password" placeholder="password" lquery="(val (** :password))" />
```

We're not quite done with that, though. We also need to supply the old password as an input to the API endpoints in order to make sure that they can check it for validity. For this we'll need another hidden input in the `actions` section:

```HTML
<input type="hidden" name="current-password" lquery="(val (** :password))" />
```

That should finally be enough to satisfy all the frontend demands. Moving on to the API endpoints, then. We don't want the password checking to happen in the `edit/delete-paste` functions, since those should be more invasive and just perform consistency checks and database operations. Instead, we'll do the password check in the actual API endpoint definitions.

```common-lisp
(defun check-password (paste password)
  (let ((paste (ensure-paste paste))
        (parent (paste-parent paste)))
    (when parent (setf paste parent))
    (when (and (= 3 (dm:field paste "visibility"))
               (string/= (cryptos:pbkdf2-hash password *password-salt*)
                         (dm:field (ensure-paste paste) "password")))
      (api-error "Invalid password for paste ~a" (dm:id paste)))))
```

We again need to take care to respect the parent relationship and instead delegate the check to that, should one exist. Finally we get to incorporate the check into the API endpoints, which should be the last we have to take care of to get proper password locking. The `plaster/new` endpoints needs the following at the beginning:

```common-lisp
(when parent (check-password parent current-password))
```
And the other two endpoints need it after the paste has been ensured:

```common-lisp
(check-password paste current-password)
```

And that's finally it. I hope. I might have missed a detail myself that presents a vulnerability in this scheme. Protecting resources against unauthorised access can be very difficult, especially when there are many different ways of accessing the same resource. I've taken care of all the situations I've been able to think of and test out, but I can't make any absolute guarantees.

## Conclusion
This was a longer part than usual, which I think nicely illustrates the complexity of safety and protection. Once you're sure that everything is fine and you've mulled things over in your mind some more, you can move on to the next part, in which we'll finally introduce user accounts to the system.

[Part 5](Part 5.md)
