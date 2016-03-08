(require 'ensime)

(push "/usr/local/bin/scala" exec-path)
(push "/usr/local/bin/sbt" exec-path)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
