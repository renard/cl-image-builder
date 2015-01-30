[![Tweet this](http://img.shields.io/badge/%20-Tweet-00aced.svg)](https://twitter.com/intent/tweet?tw_p=tweetbutton&via=renard_0&url=https%3A%2F%2Fgithub.com%2Frenard%2Fcl-image-builder&text=Easy-to-use%20%23CommonLisp%20application%20and%20runtime%20image%20builder%20using%20%23Quicklisp.)
[![Follow me on twitter](http://img.shields.io/badge/Twitter-Follow-00aced.svg)](https://twitter.com/intent/follow?region=follow_link&screen_name=renard_0&tw_p=followbutton)


# *Common Lisp* runtime image builder


`image-builder` is a small easy-to-use
[Common Lisp](http://common-lisp.net/) image builder. It works for
[Steel Bank Common Lisp](http://www.sbcl.org/) and
[Clozure Common Lisp](http://ccl.clozure.com/). You can build standalone
application like you would do with
[buildapp](http://www.xach.com/lisp/buildapp/) and custom images that fits
your needs (see [inspiration](https://gist.github.com/kisom/1548276)).

## Requirements

You need:

- a modern *Common Lisp* implementation with
  [`asdf`](http://common-lisp.net/project/asdf/) support.
- [`curl`](http://curl.haxx.se/)
- [`git`](http://git-scm.com/)

## How to

First you need to create a `.image-builder.lisp` configuration file (see
below for full description) in your project directory. This file is
mandatory, `image-builder` will fail if it it not found. Currently the
configuration file name cannot be changed.

Depending on which *Common Lisp* implementation you use, need to run
different command lines to build your application or your custom kernel
image:

- *SBCL*: `sbcl --no-sysinit --no-userinit --load image-builder.lisp`
- *CCL*: `ccl64 -n -l image-builder.lisp`

And that's it.

`Quicklisp` and all dependencies are installed in the `build` directory in
your project root folder. You won't pollute your `~/quicklisp` setup.


## Configuration file


The configuration file is a valid *Lisp S-expression*:

```common-lisp
(:custom-modules
 ((:url "https://example.com/foo/bar.git" :method :git)
  (:url "https://example.com/foo/baz.git" :method :git))
 :local-projects (:foo :foo-cli)
 :entry-point "foo-cli:main"
 :file-output "foo"
 :options-sbcl (:purify t :executable t :compression t)
 :options-ccl (:error-handler :quit :prepend-kernel t))
```

### `:custom-modules`

A list of extra modules not included in
[Quicklisp](http://www.quicklisp.org/beta/) to be downloaded into
`build/quicklisp/local-projects`.

Each item is composed of:

- `:url`: the project URL.
- `:method`: which method to use. For the moment only `git` based packages
  are supported.

### `:local-projects`

A list containing you application packages as defined in you `foo.asd`
file. See
[ASDF](http://common-lisp.net/project/asdf/asdf/Defining-systems-with-defsystem.html)
documentation for further details.

### `:entry-point`

This is the function which is called when you execute your image. If you
want to build a custom *Common Lisp* runtime image you may want to set this
option to `nil` (or not use it). If you want to build an application you
must declare a function name (please note the string type).

The entry point function is called with command line arguments as specified
by your *Common Lisp* implementation:

- *SBCL*: `sb-ext:*posix-argv*`
- *CCL*: `ccl:*command-line-argument-list*`

### `:options-scbl`

List of options to be passed to the
[`sb-ext:save-lisp-and-die`](http://www.sbcl.org/manual/#Function-sb_002dext_003asave_002dlisp_002dand_002ddie)
function when generating images for *SBCL*.

### `:options-ccl`

Same as `:options-sbcl` but for *CCL* and the
[`ccl:save-application`](http://ccl.clozure.com/manual/chapter4.9.html)
function.

## Copyright

Copyright © 2015 Sébastien Gross `<seb•ɑƬ•chezwam•ɖɵʈ•org>`.

Twitter: [@renard_0](https://twitter.com/renard_0)

License: [WTFPL](http://www.wtfpl.net)
