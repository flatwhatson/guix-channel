((nil
  (eval . (with-eval-after-load 'geiser-guile
            (let ((root-dir
                   (file-name-directory
                    (locate-dominating-file default-directory ".dir-locals.el"))))
              (unless (member root-dir geiser-guile-load-path)
                (setq-local geiser-guile-load-path
                            (cons root-dir geiser-guile-load-path))))))))
