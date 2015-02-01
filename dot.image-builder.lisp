(:asdf-file "foo.asd"
 :packages (:foo :foo-cli)
 :entry-point "foo-cli:main"
 :output-file "foo"
 :custom-systems
 ((:url "https://example.com/foo/bar.git" :method :git)
  (:url "https://example.com/foo/baz.git" :method :git))
 :options
   #+sbcl (:purify t :executable t :compression t)
   #+ccl (:error-handler :quit :prepend-kernel t)
 :build-dir "build/"
 :quicklisp-url "http://beta.quicklisp.org/quicklisp.lisp")
