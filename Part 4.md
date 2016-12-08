# Part 4
[Part 1](Part 1.md) [Part 2](Part 2.md) [Part 3](Part 3.md)

## Resources & Documentation
Here are links to relevant documentation and resource pages that will be useful to refer to for this tutorial.

* [Radiance](https://shirakumo.github.io/radiance)
* [Interface Definitions](https://github.com/Shirakumo/radiance/blob/master/standard-interfaces.lisp)
* [Clip](https://shinmera.github.io/clip)
* [lQuery](https://shinmera.github.io/lquery)
* [LASS](https://shinmera.github.io/LASS)

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

Now, with the public paste list all done, let's move on to our second task.

## Unlisting and Protecting Pastes
Another frequent feature of paste services is allowing pastes to, in the very least, be unlisted from the public view, or to be protected by a password. Especially the password requires a bit more thought, as we need to have an intermittent view that prompts for a password before allowing you to actually see the paste.

But before we get to all that, let's for a second think about the changes necessary overall.

* The editor needs a way to select whether a paste is Public, Unlisted, or Private
* The editor needs a password field if the password is private
* The viewer needs a password prompt wall to prevent unauthorised viewing
* The paste schema needs a new field for the visibility of the paste
* The listing needs to filter out unlisted and private pastes



[Part 5](Part 5.md)
