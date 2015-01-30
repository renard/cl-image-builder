(:custom-modules
 ((:url "https://example.com/foo/bar.git" :method :git)
  (:url "https://example.com/foo/baz.git" :method :git))
 :local-projects (:foo :foo-cli)
 :entry-point "foo-cli:main"
 :file-output "foo"
 :options-sbcl (:purify t :executable t :compression t)
 :options-ccl (:error-handler :quit :prepend-kernel t))
