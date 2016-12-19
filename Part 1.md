# Part 1 
[Part 0](Part 0.md)

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
Radiance has a standard project structure that we're going to follow for now. To do so, we're creating a new directory somewhere within Quicklisp's local projects, or somewhere that ASDF can find it. It should look like this:

```
plaster/
plaster/static/
plaster/template/
plaster/plaster.asd
plaster/module.lisp
plaster/frontend.lisp
```

The ASDF system definition should contain your average definition info, with some extras for Radiance's bookkeeping. Thus, `plaster.asd` should in the very least look like the following.

```common-lisp
(in-package #:cl-user)
(asdf:defsystem #:plaster
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :components ((:file "module")
               (:file "frontend"))
  :depends-on (:r-clip))
```

The `:defsystem-depends-on` and `:class` parts make sure to register the system with Radiance's modules. We'll also use `r-clip`, which is a convenience extension for the [Clip](https://shinmera.github.io/clip) template system. You can use any template system we want to for your own projects. For this, we'll go with Clip. Don't worry if you aren't familiar with it though, I'll explain it in the next section.

Next, we'll need the `module.lisp` file, which is basically an extended package definition.

```common-lisp
(in-package #:rad-user)
(define-module #:plaster
  (:use #:cl #:radiance))
```

The `define-module` macro works just as `defpackage` does. It has some additional options you can use as well, but those are not important right now. Modules are packages that allow metadata to be associated with them. Radiance makes use of this feature in a variety of ways, which is why every application written for Radiance should have a module.

Once Radiance is loaded, you can have a look at all of the currently available modules with `radiance:list-modules`. The behaviour of `describe` for modules is also extended, and will list some useful inspection information that can help you figure out what a module provides and does.

The last file that we've referenced so far is `frontend.lisp`, which you can fill with just the `in-package` statement:

```common-lisp
(in-package #:plaster)
```

Make sure to reload your ASDF/Quicklisp system cache and load your system from the REPL.

```common-lisp
(ql:register-local-projects)
(ql:quickload :plaster)
```

Next we'll want to start up Radiance. If this is your first time doing that, you'll see some welcoming messages printed on the REPL that will point you to a test page. Open it up if you like. You may ignore the notes about the configuration for the time being, we'll get into that in a later part of this series.

```common-lisp
(radiance:startup)
```

Now we're all set and ready to start all the actual work on our application.

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
        <textarea name="text" placeholder="Paste something here" autofocus required></textarea>
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

For the most part, this is standard HTML5. The only special things in the template so far are the `@href` and `@action`. These special attributes use `uri`s as their values, which are then translated and used as replacements for the `href` and `action` attribute values respectively once the template is processed. This translation is part of Radiance's routing system and ensures that links within your templates are turned into references that work on any possible server setup.

A URI in Radiance is an object that represents a trimmed down URL. It can contain a list of domains, a port number, and a path string. URIs are a central aspect and used in several places in order to handle references and represent URLs. We'll talk more about them once we get to routing.

Back to templates! Since this file is mostly just HTML, you can open it up in your browser and have a look. It's pretty barren for now, but we can fix that easily enough by writing some CSS. However, since writing plain CSS becomes pretty cumbersome fairly quick, we're going to use a CSS compiler, namely [LASS](https://shinmera.github.io/LASS). It should be fairly intuitive to realise how this translates. So, open up `plaster/static/plaster.lass` and create a file similar to this:

```common-lisp
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
  (nav :display inline-flex
       :vertical-align bottom
       (* :margin 0 10px 0 0)))
 (.infobox
  :background (rgb 100 100 100)
  :color (rgb 255 255 255)
  :padding 10px
  :margin 10px
  :font-weight bold)
 ("#error" :background (rgb 200 0 0))
 ("#message" :background (rgb 0 200 0))
 (>main
  (.edit
   :text-align right
   (header
    :text-align left
    :padding 10px
    :background (rgb 20 20 20)
    :border-bottom 3px solid (rgb 0 130 240)
    :color (rgb 255 255 255)
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
    :font-size 1.0em)
   (.actions
    :display flex
    :justify-content flex-end
    ((:or a input)
     :display inline-block)))))
```

This paste contains a tad more things than absolutely necessary for our needs right now, but in order to avoid having to paste an update for new elements in future parts, I've just inserted the completed style sheet here.

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
(define-page edit "plaster/edit" (:clip "edit.ctml")
  (r-clip:process T))

(define-page view "plaster/view/(.*)" (:uri-groups (id) :clip "view.ctml")
  (r-clip:process T))
```

A short recompile later, and visiting [the according page](http://localhost:8080/!/plaster/edit) should show you the properly rendered template. Hovering over the `New` link in the header will reveal that Clip did indeed translate the URI into a fixed URL that points to the appropriate "external" resource.

## API Endpoints and Databases
Now that we have pages going, we'll need to set up an API endpoint to paste to. In Radiance, you are encouraged to set up any kind of data manipulation action as an API endpoint rather than handling it in a page, both to separate concerns, and to make your application accessible programmatically. Since we've already referred to the API endpoint we need in our form, let's just go ahead and create it.

```common-lisp
(define-api plaster/new (text &optional title) ())
```

Since API endpoint names need to be globally unique and can't contain any parsable information, there's no need for a name and a URI like there is for a page definition. Instead, we get a simplified lambda-list that specifies the arguments that the endpoint receives, either through GET or POST variables. Last, we get another list of options, just like for the page definition.

You may be wondering why API endpoints don't allow regex-based parameters in their names like pages do. The primary reason is that doing so would ambiguate things as we already have a way to specify arguments by GET/POST parameters. Whereas for pages it makes sense to have a more human-readable URL, API endpoints are aimed at programmatical interfaces, where the construction of the parameters should be straightforward. If you really want to have parsable API endpoints you can still get them, at the cost of forsaking Radiance's integrated support for API handling. After all, you can always just define a page of your own and do absolutely anything you want with it.

So far this endpoint doesn't do anything except check that it gets the required `text` argument somehow. Not very useful. Indeed, we'll need to get access to a database to store our information in. Radiance provides for that as well, by way of an interface. Interfaces are a form of contract for the signatures of symbols in a package. What this means is that an interface is a specification that defines the signatures and behaviour of functions, variables, macros, etc. within a package. When something wants to make use of an interface's functionality, a specific implementation of that interface is then loaded.

A database interface is perhaps the most obvious and sensible example. After all, there are many different types of databases, but all of them can be used in a very similar fashion for the most part. As such it makes sense to define an interface for databases in general, and let the choice of the specific database up to someone else-- usually the administrator of a final Radiance installation.

Now, in order to make use of the database interface, we'll first need to add it to our system's dependencies.

```common-lisp
(asdf:defsystem plaster
  ...
  :depends-on ((:interface :database)
               :r-clip))
```

Reload the system, and a standard database implementation should be pulled in. It should also automatically connect to a default database instance so that we can go ahead with using it straight away. Note that if you try to load your system now after starting a fresh Lisp image, you will get an error, noting that the environment is unset. The environment is basically what decides which implementations to use for which interfaces. Since your system now depends on an interface, it can't load it without knowing this mapping. The `default` environment should be fine for now, so you can get by that error by choosing the first `continue` restart.

Now. Before we can store data away, we'll need to define a schema. Not all database implementations require a schema to operate, but some of them do. As such, you should always define a schema, even if it might be unnecessary for your specific choice of database. After all, if you're developing an application for general use, you can't constrain it to a particular setup.

When you start thinking about the schema definition, you might realise that there's a bit of a conundrum. You can't just define the database schema as a top-level form, since at the time your application is loaded, the database might not have been connected yet. The connection only happens once Radiance is started up. This is where Radiance's trigger system comes in handy. The interface for the database specifies that a hook called `database:connected` is triggered whenever the database is ready. We can make use of that.

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
(define-page view "plaster/view/(.*)" (:uri-groups (id) :clip "view.ctml")
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

First we need to parse the ID from the string parameter into an integer. Then we retrieve the record from the database. `db:query` here is a macro that recognises a rather simplified query language that all databases must support. In this case we simply check whether the `_id` field of the record matches our id variable. Every record in the database is required to have an `_id` field whose value must be unique within its collection.

Now, if the paste could not be found, an error is signalled that will usually cause an error page to be displayed to the user. Depending on your philosophy you may also want to handle that case yourself and present a special page instead of a generic error. For now, this will do. Finally, we pass some extra parameters to Clip's `process` so that we can access them from the template. Let's go and update that.

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

## Conclusion
Despite this seeming like a small goal to reach, we've touched on many different parts already now. We've looked at how to start out with a new module, how to define pages and API endpoints, how to interface and get started with a database, how to integrate Clip templates and how to use them, and finally how to use LASS for CSS compilation. If you're feeling a bit overwhelmed, don't worry. Try going through this part again and look at the documentation of the associated systems. While there are many parts, the ways in which they function are not altogether complicated.

In case you're encountering error pages and you'd like to get something more useful than a mere message, you can `(setf radiance:*debugger* T)`, which will cause the debugger to be invoked on request errors. With Slime and some looking around, I'm sure that should help you figure out what went wrong rather quickly.

We've taken our first, hopefully successful, steps building our application now. Hopefully things aren't looking too daunting to you yet. Once you think that you've understood it all well and proper enough, you can move on to the second part, in which we'll flesh things out a bit by adding some more actions. 

[Part 2](Part 2.md)
