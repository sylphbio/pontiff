(module command.new (new)

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.file)
(import chicken.file.posix)
(import chicken.pathname)
(import chicken.process)
(import chicken.process-context)

(import tabulae)
(import tabulae.monad)
(import ix)

(import (prefix state state:))
(import (prefix command.new.template template:))
(import util)

(define (new-pfile argv)
  (define pname ((^. (keyw :project-name)) argv))
  (define repo (ix:build 'pontiff:repository :name pname :vcs 'git :url ""))
  (define artifact-tag ((^.v (keyw :artifact-type)) argv))
  (define artifact (ix:build artifact-tag :name ((^. (keyw :artifact-name)) argv)
                                          :root ((^. (keyw :artifact-name)) argv)))
  ; XXX move a lot of this hardcoded shit into a global default file when I eventually do that
  (ix:build 'pontiff :name pname
                     :version '(0 1 0)
                     :pontiff-version state:version
                     :synopsis ""
                     :authors '()
                     :license "BSD-3"
                     :repository repo
                     :category 'uncategorized
                     :tags '()
                     :source-dir "src"
                     :test-dir "test"
                     :csc-flags `("-keyword-style" "prefix" "-specialize" "-local" "-lfa2"
                                  "-inline" "-inline-global" "-optimize-leaf-routines")
                     :cc 'clang
                     :ld 'lld
                     :artifacts `(,artifact)
                     :tests `(,(ix:build 'pontiff:executable :name 'unit :root 'test.unit)
                              ,(ix:build 'pontiff:executable :name 'integration :root 'test.integration))
                     :dependencies '()
                     :resolver 'alice
                     :lib-dependencies '()
                     :egg-dependencies '(test)
                     :egg-resolver 'chicken-install))

; XXX consider restructuring as, or at least to return, either
(define (new argv)
  (define project-string (symbol->string ((^.v (keyw :project-name)) argv)))

  (when (directory-exists? project-string)
        (printf "pontiff error: directory exists\n")
        (exit 1))

  (when (eq? ((^.v (keyw :artifact-name)) argv) 'test)
        (printf "pontiff error: test is a reserved module name\n")
        (exit 1))

  (define pfile (new-pfile argv))
  (define srcdir ((^.v (keyw :source-dir)) pfile))
  (define tstdir ((^.v (keyw :test-dir)) pfile))

  (define tmpdir (create-temporary-directory))
  (change-directory tmpdir)

  (create-directory srcdir)
  (create-directory tstdir)
  (create-directory (state:build-dir))

  (for-each (lambda (a) (let* ((a-root (symbol->string ((^.v (keyw :root)) a)))
                               (a-body (string-translate* (template:tag->template (ix:ident->tag ((^. ident) a)))
                                                          `(("##ROOT##" . ,a-root)))))
                              (save-file (make-pathname srcdir a-root "scm") a-body)))
            ((^.v (keyw :artifacts)) pfile))

  (for-each (lambda (a) (let* ((a-root (symbol->string ((^.v (keyw :root)) a)))
                               (a-file (if (equal? (take* 5 a-root) "test.") (drop* 5 a-root) a-root))
                               (a-body (string-translate* template:test `(("##ROOT##" . ,a-root)))))
                              (save-file (make-pathname tstdir a-file "scm") a-body)))
            ((^.v (keyw :tests)) pfile))

  (state:save-pfile pfile)

  ; XXX add a vcs flag that lets you set none, make the field optional, gate this
  (save-file ".gitignore" template:gitignore)
  ; XXX FIXME very rude to hardcode the filename but this repo shit needs to move into state probably
  (system* (sprintf "git init && git add ~S ~S pontiff.ix .gitignore && git commit -m 'pontiff new'" srcdir tstdir))

  (change-directory (state:working-path))
  ; XXX this is really annoying but move-file doesn't work on directories and rename-file doesn't work across devices
  (system* (sprintf "mv ~S ~S" tmpdir project-string))
  (printf "created new project ~S\n" project-string))

)
