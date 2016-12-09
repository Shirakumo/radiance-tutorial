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

(define-page list "plaster/list(/(.*))?" (:uri-groups (NIL page) :lquery "list.ctml")
  (let* ((page (or (when page (parse-integer page :junk-allowed T)) 0))
         (pastes (dm:get 'plaster-pastes (db:query :all)
                         :sort '((time :DESC))
                         :skip (* page *pastes-per-page*)
                         :amount *pastes-per-page*)))
    (r-clip:process T :pastes pastes
                      :page page
                      :has-more (<= *pastes-per-page* (length pastes)))))
```

To get the pagination going we use the `:skip` and `:amount` arguments. The `:sort` clause ensures deterministic ordering of the results. Finally we use a dynamic variable to hold how many pastes we want to display on each page and supply the template with the necessary stuff.

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

We allow both string and integer arguments for the visibility here in order to make the API a bit more "human". For the hashing of the password we've employed the help of another library called [Crypto-Shortcuts](https://shinmera.github.io/crypto-shortcuts), which should also be added to the ASDF system. This is also the first occurrence of the `or*` macro, which is just the same as `or`, but treats an empty string as NIL, which is a very useful behaviour for GET and POST arguments on web interfaces.

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

You should now be able to paste with the different visibilities being remembered and checked properly. However, the actual behaviour on the list page and the view page are not yet implemented.


[Part 5](Part 5.md)
