# Part 1 
## Introduction
While many things in Radiance will work very similarly to other web frameworks, others will not. Either way, you should probably either follow the tutorial along step by step, or in the very least keep the [documentation](https://shirakumo.github.io/radiance) open to reference functions alongside.

In this tutorial we're going to build a fully-fledged paste service from the ground up. I'm going to show this in the way that any other application might be designed. As such, we're going to start out simple and expand from there, testing things along the way.

It is expected that you have a moderate understanding and amount of experience with Common Lisp, know how to create basic projects, and how to manage systems, dependencies, and packages.

This tutorial was written for Radiance 1.0; it may not work for later versions.

## A Short Roadmap
A first step should probably be to consider what kind of capabilities the application should have and in what way they are presented to the user. A paste service is relatively clearly defined, so this isn't much of a problem.

In the end, we'll want:

* A capable editor to write text with (syntax highlighting)
* A way to post the text under a unique and short URL
* Being able to post the texts both anonymously and under a profile
* Some kind of spam protection
* Seeing pastes on people's profile and a public list
* Allowing the pastes to be protected by a password and unlisted
* Being able to access everything programmatically through a REST API
* A way to edit and delete pastes after the fact

The first step however should be much more minimal. We'll settle with the following:

* A site with a text area that allows you to post
* A site that allows you to view a previous post

We'll make short mission statements like that for each part of this tutorial to outline what we want to have accomplished by the end of it.

## Preparing a Module
Radiance has a standard project structure that we're going to follow for now. So, we're creating a new directory somewhere within Quicklisp's local projects or somewhere that ASDF can find it like so:

```
plaster/
plaster/static/
plaster/template/
plaster/plaster.asd
plaster/module.lisp
plaster/frontend.lisp
```

The ASDF system definition should contain your average definition info, with some extras for Radiance's bookkeeping. Thus, `plaster.asd` should contain the following:

```common-lisp
(in-package #:cl-user)
(asdf:defsystem #:plaster
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :components ((:file "module")
               (:file "frontend"))
  :depends-on (:r-clip))
```

The `:defsystem-depends-on` and `:class` parts make sure to register the system with Radiance's modules. We'll also use `r-clip`, which is a convenience extension for the [Clip](https://shinmera.github.io/clip) template system. You can use any template system we want to for your own projects. For this, we'll go with Clip.

Next, we'll need the `module.lisp` file, which is basically an extended package definition.

```common-lisp
(in-package #:rad-user)
(define-module #:plaster
  (:use #:cl #:radiance))
```

The `define-module` macro works just as `defpackage` does, but allows some extensible extra options for modules and makes sure to register the package as a proper module within Radiance.

## Designing a Template
Often times it's a good idea to design the user interface first. Doing so will give you a good idea of what exactly you will need to support in your backend and what it will all look like in the end. Clip's main strength lies in exactly this-- it allows you to write templates that you can view in the browser directly without any processing necessary. This means you can write your page without needing to write a single piece of code. I don't expect you to be familiar with Clip, so I'll give a short crash course here as well. For more information, see its [documentation](https://shinmera.github.io/clip).

Create a file `plaster/template/edit.ctml` and edit it to contain something similar to this.

```html
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
      <form class="edit" method="post" action="#" @action="/api/plaster/new">
        <header>
          <input type="text" name="title" placeholder="Untitled" maxlength="32" />
        </header>
        <textarea name="text" placeholder="Paste something here" autofocus></textarea>
        <nav class="actions">
          <input type="hidden" name="browser" value="true" />
          <input type="submit" name="action" value="Post" />
        </nav>
      </form>
    </main>
    <footer>
    </footer>
  </body>
</html>
```

For the most part this is standard HTML5. The only special things in the template so far are the `@href` and `@action`. These special attributes use URIs as their values, which are then translated and used as replacements for the `href` and `action` attribute values respectively once the template is processed. This translation is part of Radiance's routing system and ensures that links within your templates are turned into references that work on any possible server setup.

Since this file is mostly just HTML, you can open it up in your browser and have a look. It's pretty barren for now, but we can fix that easily enough by writing some CSS. However, since writing plain CSS becomes pretty cumbersome fairly quick, we're going to use a CSS compiler, namely [LASS](https://shinmera.github.io/LASS). It should be fairly intuitive to realise how this translates. So, open up `plaster/static/plaster.lass` and create a file similar to this:

```LASS
(body
 :font-family sans-serif
 :font-size 14pt
 :background (rgb 0 0 0)
 :margin 0
 ((:or a input[type=submit])
  :text-decoration none
  :background (rgb 0 130 240)
  :color (rgb 255 255 255)
  :padding 2px 5px 2px 5px
  :border none
  :font-size 1.0em
  :font-weight bold)
 ((:and (:or a input[type=submit]) ":hover")
  :background (rgb 255 255 255)
  :color (rgb 0 0 0)
  :cursor pointer)
 (>header
  :color (rgb 255 255 255)
  (h1 :margin 0 20px 0 20px
      :font-size 2em
      :display inline)
  (nav :display inline-flex))
 (>main
  (.edit
   :text-align right
   (header
    :text-align left
    :padding 10px
    :background (rgb 20 20 20)
    :border-bottom 3px solid (rgb 0 130 240)
    (input[type=text]
     :font-size 1.0em
     :width 400px
     :background (rgb 50 50 50)
     :color (rgb 255 255 255)
     :border none
     :padding 5px))
   (textarea
    :width 100%
    :padding 5px
    :min-height 200px
    :box-sizing border-box
    :display block
    :font-family monospace
    :font-size 1.0em))))
```

If you load `lass.el` into your emacs setup before opening the file, it'll compile it to the according CSS file automatically whenever you save. Otherwise, load LASS into your lisp and compile the file.

```common-lisp
(ql:quickload :lass)
(lass:generate (asdf:system-relative-pathname :plaster "static/plaster.lass"))
```

You should now have a `plaster.css` alongside the LASS file. The HTML template we made above should already reference that file, so just refresh the file in your browser to see what it looks like.

We're almost done now. We just need a second template for the viewing of a paste. It should probably look very similar to the edit page. In fact, it's practically the same except for some very minor changes.

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
      <form class="edit">
        <header>
          <input type="text" name="title" placeholder="Untitled" maxlength="32" readonly />
        </header>
        <textarea name="text" placeholder="Paste something here" readonly></textarea>
        <nav class="actions"></nav>
      </form>
    </main>
    <footer>
    </footer>
  </body>
</html>
```

The rationale for re-using the form and just making the fields `readonly` is that we can re-use the CSS file verbatim, and that it will be convenient for additions later on, when we add actions like repasting, editing, annotating, and so forth.

## Setting Up Pages
Now that we have the necessary templates worked out, we can start with our actual application. To begin with we'll want two pages, one to make a paste, and one to view a paste. To do this, we'll populate the `frontend.lisp` like so.

```common-lisp
(in-package #:plaster)

(define-page edit "plaster/edit" ())

(define-page view "plaster/view/(.*)" (:uri-groups (id)))
```

As they stand, the definitions won't actually deliver any content yet. Don't worry, we'll get around to that in a minute. For now, let's just see what this does.

In Radiance, the code that is executed to satisfy any particular request is decided by a mechanism called "uri dispatching". Therein a request's target URL is compared against a sequence of dispatchers. As soon as a dispatcher's URI matches gainst the request, the dispatcher is executed and the dispatching ends.

For our first page, `edit`, we define `plaster/edit` which decomposes into an URI with one subdomain, `plaster`, and a path component of `edit`. For the second page, `view`, the resulting URI has the same subdomain, but its path component is a bit more complex as `view/(.*)` is a regular expression. The path component is the only part of a URI that can contain a regular expression.

Following the URI is a list of page options, which is used in the `view` page to define a capture variable for the regex group. In the page's body we can now refer to the value captured by the group with the `id` variable.

You may be wondering why we're using a subdomain for both of our URIs. Radiance's convention is that each application should put all of its pages onto a subdomain named after the module. This avoids pages from clashing with each other. If you don't want to require the user to access the application through a subdomain, you can fix that later by using a route. We'll get to routing in a later part. For now, just know that there's a good reason for this convention.

Now we'll want to actually process the templates in the pages. This does not require much, for now. Simply change the definitions to look like this: 

```common-lisp
(define-page edit "plaster/edit" (:lquery "edit.ctml")
  (r-clip:process T))

(define-page view "plaster/view/(.*)" (:uri-groups (id) :lquery "view.ctml")
  (r-clip:process T))
```

A short recompile later, and visiting [the according page](https://localhost:8080/!/plaster/edit) should show you the properly rendered page. Hovering over the `New` link in the header will reveal that Clip did indeed translate the URI into a fixed URL that points to the appropriate "external" resource.

## API Endpoints and Databases
Now that we have pages going, we'll need to set up an API endpoint to paste to. In Radiance, you are encouraged to set up any kind of data manipulation action as an API endpoint rather than handling it in a page, both to separate concerns, and to make your application accessible programmatically. Since we've already referred to the API endpoint we need in our form, let's just go ahead and create it.

```common-lisp
(define-api plaster/new (text &optional title) ())
```

Since API endpoint names need to be globally unique and can't contain any parsable information, there's no need for a name and a URI like there is for a page definition. Instead, we get a simplified lambda-list that specifies the arguments that the endpoint receives, either through GET or POST variables. Last, we get another list of options, just like for the page definition.

So far this endpoint doesn't do anything except check that it gets the required `text` argument somehow. Not very useful. Indeed, we'll need to get access to a database to store our information in. Radiance provides for that as well, by way of an interface. In order to make use of the interface, we'll first need to add it to our system's dependencies.

```common-lisp
(asdf:defsystem plaster
  ...
  :depends-on ((:interface :database)
               :r-clip))
```

Reload the system, and a standard database implementation should be pulled in. It should also automatically connect to a default database instance so that we can go ahead with using it straight away. First we'll need to define a schema. Database backends are not required to respect a schema, but some backends require you to define one. As such, you should always define a schema, even if it might be unnecessary for some.

However, you might realise that there's a bit of a conundrum. You can't just define the database schema as a top-level form, since at the time your application is loaded, the database might not have been connected yet. This is where Radiance's trigger system comes in handy. The interface for the database specifies that a hook called `database:connected` is triggered whenever the database is ready.

```common-lisp
(define-trigger db:connected ()
  (db:create 'plaster-pastes '((title (:varchar 32))
                               (time (:integer 5))
                               (text :text))))
```

If you compile the trigger after the database has already been connected, it will be triggered automatically, since the hook is a "switch hook" that automatically calls new trigger definitions if it has already been switched on. With our schema at the ready, we can modify the API endpoint to insert the data.

```common-lisp
(define-api plaster/new (text &optional title) ()
  (let ((id (db:insert 'plaster-pastes `((title . ,title)
                                         (time . ,(get-universal-time))
                                         (text . ,text)))))
    (if (string= "true" (post/get "browser"))
        (redirect (make-uri :domains '("plaster")
                            :path (format NIL "view/~a" id)))
        (api-output `(("id" . ,id))))))
```

So, what's going on here? We're inserting a new record into our collection, with the required fields according to the schema. Returned by that is the new ID of the record. We then check whether the endpoint was visited with a browser, and based on that either redirect to the view page, or present the ID as data in a machine-readable fashion.

So far so good. Now we need to update the view page to actually retrieve the requested paste and put the data from that into the template. Let's start with the page.

```common-lisp
(define-page view "plaster/view/(.*)" (:uri-groups (id) :lquery "view.ctml")
  (let* ((id (parse-integer id))
         (paste (first (db:select 'plaster-pastes (db:query (:= '_id id)) :amount 1))))
    (unless paste
      (error 'request-not-found :message (format NIL "No paste with ID ~a was found." id)))
    (r-clip:process
     T
     :title (gethash "title" paste)
     :time (gethash "time" paste)
     :text (gethash "text" paste))))
```

First we need to parse the ID from the string parameter into an integer. Then we retrieve the record from the database. If none could be found, an error is signalled that will usually cause an error page to be displayed to the user. Depending on your philosophy you may also want to handle that case yourself and present a special page instead of a generic error. For now, this will do. Finally, we pass some extra parameter's to Clip's `process` so that we can access them from the template. Let's go and update that.

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
      <form class="edit">
        <header>
          <input type="text" name="title" placeholder="Untitled" maxlength="32" readonly
                 lquery="(val title)" />
        </header>
        <textarea name="text" placeholder="Paste something here" readonly
                  lquery="(text text)"></textarea>
        <nav class="actions"></nav>
      </form>
    </main>
    <footer>
    </footer>
  </body>
</html>
```

The only things changed are the `input` and `textarea` elements, which now have an `lquery` attribute. This attribute can contain a sequence of [lQuery](https://shinmera.github.io/lquery) instructions to modify the element. In this case, we're just using it to conveniently fill in the value attribute and set the text content.

You should now be able to visit [the /new page](http://localhost:8080/!/plaster/new), create a paste, and view it. And with that we have achieved the goals we've set for the first part of this tutorial.

Despite this seeming like a small goal to reach, we've touched on many different parts already now. We've looked at how to start out with a new module, how to define pages and API endpoints, how to interface and get started with a database, how to integrate Clip templates and how to use them, and finally how to use LASS for CSS compilation. If you're feeling a bit overwhelmed, don't worry. Try going through this part again and look at the documentation of the associated systems. While there are many parts, the ways in which they function are not altogether complicated.

Once you think that you've understood it all well and proper enough, you can move on to the second part, in which we'll flesh things out a bit by adding paste editing, deleting, and validation.
