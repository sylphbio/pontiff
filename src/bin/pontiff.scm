(module bin.pontiff ()

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.process-context)

(import tabulae)
(import tabulae.monad)
(import ix)

(import (prefix state state:))
(import (prefix argv argv:))
(import (prefix command command:))

; XXX COOL the "link bug" was a chicken compiler bug NEXT STEPS
; * impl ix generic, fix modules.ix
;   - key: module name and static/dynamic, value: sg hash and whether toplevel
;   - track whether an exe was last linked statically or dynamically
;   - track whether a lib was last linked by a subinvocation
; * write bootstrapping script... nothing fancy, trivial scheme script with no dependencies
;   - module order up top. ideally when I update pontiff, I just have to fix this
;     find all in src and confirm the list is exclusive and exhaustive (aside from ix/tab modules)
;   - clone ix and tabulae (to a temp dir) symlink the files
;   - chicken-install all necessary eggs to system repo
;   - foreach over everything compiling in serial. dynamic is fine so I don't have to care about -uses
;   and I guess that's basically it. link, and if I want to be super fancy use the result to build again (static)
; * deps.ix, just record what all I've gathered after I finish
;   gather should only do anything when the pontiff file dep/egg lists are changed, or when forced
; * simple readme, add license file
; and then I think I'm good to release. nice future things...
; * __VERSION__ __LINE__ etc replacements. possibly by user compiler pass? pontiff language extensions lol
; * our own import files, so we can fully parallelize compilation
;   maybe a declare that indicates no macros are exported...
;   complicated tho, we need to parse existing import files to resolve reexports
; * tests lol
; * vcs behind a generic interface
; * centralize hardcoded values in some module
; * command.repl, command.release
; * impl resolver lol
; * link external deps. I need:
;   - declare what libraries a pontiff project itself needs linked
;   - determine what libraries eggs depend on that need to be statically linked
;     it appears chicken-install doesn't do this. quick testing confirms csc building a static executable doesn't work
;     this may be academic for now, apparantly nanomsg itself doesn't even work as a static lib
; * build embedded. basically just set -DC_EMBEDDED. examples just have a c file that includes chicken.h and pass it to csc too
; * cross-compilation. do not fuck with this until pontiff manages its own compilers tho
; and aside from that I need to do ix/json conversions and rewrite the sbml parser for viv
; XXX I'm a genius the "safe" way to do pontiff upgrades is write functional mappings for pontiff files across versions
; and then just depclean the fucking project whenever the pontiff file version and pontiff version don't match lmao
; XXX should I just like, add a prologue that forces a proper repo string in dynamic builds?
; the fucky thing here is... if I set it for pontiff, can pontiff use its own extensions and properly set those of its build?
; hmm I... guess it only matters for dynamic executables, and I *think* I only "use" it for chicken-install calls
; so if I bake in CHICKEN_REPOSITORY_PATH in pontiff it should work fine and state:env ignores it for PONTIFF_LINK_PATH anyway

(define (main)
  (state:init)

  (define ret (do/m <either>
    (args <- (argv:process (command-line-arguments)))
    (command:dispatch args)))

  (when (left? ret)
        (printf (<> (cdadr ret) "\n"))
        (exit (caadr ret)))

  (exit 0))

(main)

)
