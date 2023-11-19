(require 'ert)
(require 'scala-cli-repl)

(ert-deftest scala-cli-convert-sbt-dependencies-test ()
  (should
   (equal
    (scala-cli-convert-sbt-dependencies "\"org.typelevel\" %% \"cats-effect\" % \"3.5.2\"" 'identity)
    "org.typelevel::cats-effect:3.5.2")))
