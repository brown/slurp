
;;;;    slurp.lisp

(in-package #:slurp)

;; http://github.com/7max/log4cl
;;web: http://git.androdna.com/?p=lisp/package-aliases.git
;;git: http://git.androdna.com/git/lisp/package-aliases.git
;; > http://sf.net/projects/tmasterkey2/
;; github.com/Neronus/clesh
;; https://github.com/kruhft/cl-active-variables
;; http://www.kylheku.com/cgit/lisp-snippets/tree/refs.lisp
;; http://mustache.github.com/    https://github.com/osa1/cl-mustache
;; https://github.com/vsedach/cliki2
;; https://github.com/archimag/cliki2/issues
;; https://github.com/AccelerationNet/static-analysis/blob/master/static-analysis.lisp
;; https://github.com/pkhuong/Napa-FFT
;; https://github.com/filonenko-mikhail/maxima/tree/quicklisp
;;   http://code.google.com/p/cl-op/
;; github.com/Hexstream
;; github.com/luisbmo
;; http://jp-larocque.livejournal.com/66618.html
;; https://github.com/nikodemus/sb-texinfo
;; http://github.com/galdor/m2cl
;; https://github.com/gigamonkey/toot/
;; https://github.com/stassats/lisp-config/blob/master/bin/data
;; https://github.com/quicklisp/quicklisp-projects

;; http://code.google.com/p/cl-gdata/source/browse/src/contacts.lisp
;; git://matlisp.git.sourceforge.net/gitroot/matlisp/matlisp
;; https://github.com/galdor/cl-zmq
;; http://codemore.org/cl-zmq.html
;; https://github.com/mmontone

;; https://github.com/drewc/smug
;; https://github.com/rpav/cl-xcb-xlib
;; https://github.com/sharplispers
;; https://github.com/mtravers/waybacker/blob/master/src/oauth2-google.lisp
;; cvs -z3 -d:pserver:anonymous@clorb.cvs.sourceforge.net:/cvsroot/clorb co clorb
;; https://github.com/mathematical-systems/
;; all pjb stuff may be here:
;;  https://gitorious.org/com-informatimago/com-informatimago/trees/master/common-lisp/lisp-reader

(defparameter *source-root* "/local/software/source-trees"
  "Directory into which source code repositories are checked out.")

(defparameter *systems-root* "/local/software/systems"
  "Directory populated with symbolic links to repository ASDF files.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun concat (&rest args)
    (apply #'concatenate (cons 'string args))))

(defparameter +repository-specs+
  '((3b-swf (github "3b")
     :asd ("3b-swf-swc.asd"
           "3b-swf.asd"))

    ;; All these are duplicates ... sort out !!!
    ;; (bordeaux-threads (github "sionescu"))
    ;; (commonqt (github "stassats"))
    ;; (iolib (github "sionescu"))
    ;; (static-vectors (github "sionescu"))
    ;; (split-sequence (github "sionescu"))
    ;; (versioned-objects (github "smithzvk" "Versoned-Objects")
    ;;  :asd ("versioned-objects.asd"
    ;;        "versioned-objects-test.asd"))


    (3bil (github "3b")
     :asd ("3b-swf-writer.asd"
           "avm2-asm.asd"
           "avm2-compile.asd"
           "avm2-lib.asd"
           "swf-writer-hack.asd"))
    (3bmd (github "3b"))
    ;; The following URL was recently announced.  Is fetching ABCL via http
    ;; better?  svn co http://svn.common-lisp.net/armedbear/trunk abcl
    (abcl svn "svn://common-lisp.net/project/armedbear/svn/trunk/abcl")
    (abcl-helper (github "quek"))
    (abcl-web (sourceforge svn)
     :asd none)
    (acl-zmq (github "marijnh"))
    (adw-charting (clnet darcs)
     :asd ("adw-charting-google.asd"
           "adw-charting-vecto.asd"
           "adw-charting.asd"))
    (aftpd (github "franzinc")
     :asd none)
    ;; XXXX: asd file name conflicts with asdf-install
    (ait (clnet darcs)
     :asd ("asdf-install-tester.asd"
           "asdf-install-dates.asd"))
    ;; http://common-lisp.net/project/alexandria/
    (alexandria (clnet git)
     :asd ("alexandria.asd"
           "alexandria-tests.asd"))
    (alref (github "adlai" "ALREF"))
    (amazon-ecs (github "gonzojive"))
    (amd64-asm (google-code svn))
    (anaphora (clnet cvs "anaphora" "src"))
    (anardb git "http://cl-www.msi.co.jp/projects/anardb/anardb.git"
     :asd ("anardb.asd"
           "anardb-test.asd"))
    (andes (github "bvds")
     :asd ("andes.asd"
           "andes-help.asd"
           "web-server.asd"))
    (ansi-tests svn "svn://common-lisp.net/project/ansi-test/svn/trunk/ansi-tests"
     :asd none)
    (antiweb (github "hoytech")
     :asd none)
    (araneida-release darcs "http://common-lisp.net/project/araneida/araneida-release"
     :asd none)
    (araneida-testing darcs "http://common-lisp.net/project/araneida/araneida-testing"
     :asd ("araneida.asd"))
    (arc-compat (github "g000001"))
    ;; hanshueber has a fork of archive on github.
    (archive (github "froydnj"))
    (armish (clnet darcs))
    (arnesi_dev darcs "http://common-lisp.net/project/bese/repos/arnesi_dev"
     :asd ("arnesi.asd"))
    (aromyxo (github "lnostdal" "Aromyxo"))
    (array-operations (github "tpapp"))
    (artificial-flavors (github "g000001"))
    (asdf (clnet git)
     :asd none)
    (asdf-binary-locations darcs "http://common-lisp.net/project/asdf-binary-locations/darcs"
     :asd ("asdf-binary-locations-test.asd"
           "asdf-binary-locations.asd"
           "test/test-force.asd"
           "test/test1.asd"
           "test/test2.asd"
           "tests/abl-test-system.asd"))
    (asdf-install (github "gwkkwg")
     :asd ("asdf-install/asdf-install.asd"
           "asdf-install/test-asdf-install.asd"))
    ;; XXXX: The documenation for asdf-install no longer mentions this
    ;; repository as the place for pre-release versions.  It's probably
    ;; obsolete.
    (asdf-install-unstable
     darcs "http://common-lisp.net/project/asdf-install/asdf-install-unstable"
     :asd none)
    (asdf-system-connections
     darcs #.(concat "http://common-lisp.net/project/cl-containers/"
                     "asdf-system-connections/darcs/asdf-system-connections"))
    (asdf-world (melis))
    (aserve (github "franzinc")
     :asd none)
    (aspectl darcs "http://common-lisp.net/project/closer/repos/aspectl")
    (autobench (github "antifuchs")
     :asd ("autobench.asd"
           "autobench-ht.asd"
           "web/autobench-web.asd"))
    (autoproject (google-code svn)
     :asd ("autoproject.asd"
           "autoproject.crud.asd"
           "autoproject.pkg.asd"
           "autoproject.util.asd"))
    (avl-tree (github "vy"))
    ;; XXXX Axiom has transitioned to git for version control, but which repository is canonical?
    ;; The web site lists at least:
    ;;   git clone git://axiom.git.sourceforge.net/gitroot/axiom/axiom
    ;;   git clone git://git.savannah.nongnu.org/axiom.git
    ;;   git clone axiom@git.sv.nongnu.org:/srv/git/axiom.git
    ;; Figure out which is best.  Bill Daly's git repository on github and the one on sourceforge
    ;; are getting updates.
    (axiom (github "daly" "axiom")
     :asd none)
    (babel (clnet darcs)
     :asd ("babel.asd"
           "babel-streams.asd"
           "babel-tests.asd"))
    (bayescl (clnet cvs))
    (bdb (clnet darcs))
    (bdb-playground darcs "http://common-lisp.net/project/bdb/darcs/bdb-playground")
    (beirc (clnet cvs))
    ;; XXXX: I think the second is probably canonical now, but it lacks two files --
    ;; pdf and ps -- look at them.
    ;; (binary-types cvs pserver anonymous t common-lisp.net "/project/movitz/cvsroot")
    (binary-types (github "frodef"))
    (binascii (github "froydnj")
     :asd none)
    (binomial-heap (github "vy"))
    (bk-tree (github "vy"))
    ;; The bknr reporsitory holds the canonical version of many ediware libraries.  I have also
    ;; retained mirrors of the ediware libraries elsewhere, since they occasionally are slightly
    ;; different.
    (bknr svn "svn://svn.bknr.net/svn/trunk"
     :asd ("libraries/clixdoc/clixdoc.asd"
           "libraries/xhtmlgen/xhtmlgen.asd"
           "libraries/yason/yason.asd"
           "projects/album-maker/src/album-maker.asd"
           "projects/bknr-website/src/bknr.website.asd"
           "projects/bos/m2/bos.m2.asd"
           "projects/bos/test/bos.test.asd"
           "projects/bos/web/bos.web.asd"
           "projects/hello-web/src/hello-web.asd"
           "projects/lisp-ecoop/src/lisp-ecoop.asd"
           "projects/mah-jongg/src/mah-jongg.asd"
           "projects/poll-postbank/poll-postbank.asd"
           "projects/quickhoney/src/quickhoney.asd"
           "projects/scrabble/src/scrabble.asd"
           "projects/unmaintained/eboy/src/eboy.asd"
           "projects/unmaintained/gpn/gpn.asd"
           "projects/unmaintained/raw-data/mcp/mcp.asd"
           "projects/unmaintained/saugnapf/src/saugnapf.asd"
           "thirdparty/documentation-template/documentation-template.asd"
           "thirdparty/flexi-streams/flexi-streams.asd"
           ;; "thirdparty/alexandria/alexandria-tests.asd"
           ;; "thirdparty/alexandria/alexandria.asd"
           ;; "thirdparty/anaphora-0.9.3/anaphora.asd"
           ;; "thirdparty/arnesi/arnesi.asd"
           ;; "thirdparty/asdf-system-connections/asdf-system-connections.asd"
           ;; "thirdparty/asdf/asdf.asd"
           ;; "thirdparty/asdf/test/file3-only.asd"
           ;; "thirdparty/asdf/test/graveyard/test-preferences-system-1.asd"
           ;; "thirdparty/asdf/test/static-and-serial.asd"
           ;; "thirdparty/asdf/test/test-force.asd"
           ;; "thirdparty/asdf/test/test-missing-lisp-file.asd"
           ;; "thirdparty/asdf/test/test-module-depend.asd"
           ;; "thirdparty/asdf/test/test-module-excessive-depend.asd"
           ;; "thirdparty/asdf/test/test-module-pathnames.asd"
           ;; "thirdparty/asdf/test/test-modules-serial.asd"
           ;; "thirdparty/asdf/test/test-modules.asd"
           ;; "thirdparty/asdf/test/test-nested-components-1.asd"
           ;; "thirdparty/asdf/test/test-package.asd"
           ;; "thirdparty/asdf/test/test-redundant-recompile.asd"
           ;; "thirdparty/asdf/test/test-samedir-modules.asd"
           ;; "thirdparty/asdf/test/test1.asd"
           ;; "thirdparty/asdf/test/test2.asd"
           ;; "thirdparty/asdf/test/test2a.asd"
           ;; "thirdparty/asdf/test/test2b1.asd"
           ;; "thirdparty/asdf/test/test2b2.asd"
           ;; "thirdparty/asdf/test/test2b3.asd"
           ;; "thirdparty/asdf/test/test3.asd"
           ;; "thirdparty/asdf/test/test5.asd"
           ;; "thirdparty/asdf/test/test9-1.asd"
           ;; "thirdparty/asdf/test/test9-2.asd"
           ;; "thirdparty/asdf/test/try-recompiling-1.asd"
           ;; "thirdparty/asdf/test/try-reloading-1.asd"
           ;; "thirdparty/asdf/test/wild-module.asd"
           ;; "thirdparty/atdoc/atdoc.asd"
           ;; "thirdparty/atdoc/example/blocks-world.asd"
           ;; "thirdparty/babel/_darcs/pristine/babel-streams.asd"
           ;; "thirdparty/babel/_darcs/pristine/babel-tests.asd"
           ;; "thirdparty/babel/_darcs/pristine/babel.asd"
           ;; "thirdparty/babel/babel-streams.asd"
           ;; "thirdparty/babel/babel-tests.asd"
           ;; "thirdparty/babel/babel.asd"
           ;; "thirdparty/bordeaux-threads/bordeaux-threads.asd"
           ;; "thirdparty/cffi/cffi-examples.asd"
           ;; "thirdparty/cffi/cffi-grovel.asd"
           ;; "thirdparty/cffi/cffi-tests.asd"
           ;; "thirdparty/cffi/cffi-uffi-compat.asd"
           ;; "thirdparty/cffi/cffi.asd"
           ;; "thirdparty/cffi/uffi-compat/uffi.asd"
           ;; "thirdparty/chtml/closure-html.asd"
           ;; "thirdparty/chunga/chunga.asd"
           ;; "thirdparty/cl+ssl/cl+ssl.asd"
           ;; "thirdparty/cl-base64/cl-base64.asd"
           ;; "thirdparty/cl-fad/cl-fad.asd"
           ;; "thirdparty/cl-ftp/ftp.asd"
           ;; "thirdparty/cl-gd/cl-gd-test.asd"
           ;; "thirdparty/cl-gd/cl-gd.asd"
           ;; "thirdparty/cl-interpol/cl-interpol.asd"
           ;; "thirdparty/cl-mime/cl-mime.asd"
           ;; "thirdparty/cl-paypal/cl-paypal.asd"
           ;; "thirdparty/cl-pdf/cl-pdf-parser.asd"
           ;; "thirdparty/cl-pdf/cl-pdf.asd"
           ;; "thirdparty/cl-ppcre/cl-ppcre-unicode.asd"
           ;; "thirdparty/cl-ppcre/cl-ppcre.asd"
           ;; "thirdparty/cl-qprint/cl-qprint.asd"
           ;; "thirdparty/cl-smtp/cl-smtp.asd"
           ;; "thirdparty/cl-ssl/cl-ssl/cl-ssl.asd"
           ;; "thirdparty/cl-stm/_darcs/pristine/cl-stm.asd"
           ;; "thirdparty/cl-stm/cl-stm.asd"
           ;; "thirdparty/cl-store_0.8.4/cl-store.asd"
           ;; "thirdparty/cl-unicode/cl-unicode.asd"
           ;; "thirdparty/cl-vectors-0.1.3/cl-aa-misc.asd"
           ;; "thirdparty/cl-vectors-0.1.3/cl-aa.asd"
           ;; "thirdparty/cl-vectors-0.1.3/cl-paths-ttf.asd"
           ;; "thirdparty/cl-vectors-0.1.3/cl-paths.asd"
           ;; "thirdparty/cl-vectors-0.1.3/cl-vectors.asd"
           ;; "thirdparty/cl-webdav/cl-webdav.asd"
           ;; "thirdparty/cl-who/cl-who.asd"
           ;; "thirdparty/cl-xmlspam/cl-xmlspam.asd"
           ;; "thirdparty/cl-yacc/_darcs/pristine/yacc.asd"
           ;; "thirdparty/cl-yacc/yacc.asd"
           ;; "thirdparty/closer-mop/_darcs/current/closer-mop.asd"
           ;; "thirdparty/closer-mop/closer-mop.asd"
           ;; "thirdparty/closure-common/closure-common.asd"
           ;; "thirdparty/closure-html/closure-html.asd"
           ;; "thirdparty/cxml-stp/cxml-stp.asd"
           ;; "thirdparty/cxml/cxml.asd"
           ;; "thirdparty/cybertiggyr-time/cybertiggyr-time.asd"
           ;; "thirdparty/defclass-star/_darcs/pristine/defclass-star.asd"
           ;; "thirdparty/defclass-star/defclass-star.asd"
           ;; "thirdparty/drakma/drakma.asd"
           ;; "thirdparty/fiveam/_darcs/pristine/fiveam.asd"
           ;; "thirdparty/fiveam/fiveam.asd"
           ;; "thirdparty/hunchentoot/hunchentoot.asd"
           ;; "thirdparty/iolib/examples/iolib.examples.asd"
           ;; "thirdparty/iolib/src/iolib.asd"
           ;; "thirdparty/iolib/src/iolib.base.asd"
           ;; "thirdparty/iolib/src/iolib.multiplex.asd"
           ;; "thirdparty/iolib/src/iolib.os.asd"
           ;; "thirdparty/iolib/src/iolib.pathnames.asd"
           ;; "thirdparty/iolib/src/iolib.sockets.asd"
           ;; "thirdparty/iolib/src/iolib.streams.asd"
           ;; "thirdparty/iolib/src/iolib.syscalls.asd"
           ;; "thirdparty/iolib/src/iolib.trivial-sockets.asd"
           ;; "thirdparty/iolib/src/iolib.zstreams.asd"
           ;; "thirdparty/iolib/tests/iolib-tests.asd"
           ;; "thirdparty/ironclad/ironclad.asd"
           ;; "thirdparty/iterate/_darcs/pristine/iterate.asd"
           ;; "thirdparty/iterate/iterate.asd"
           ;; "thirdparty/kmrcl-1.97/kmrcl-tests.asd"
           ;; "thirdparty/kmrcl-1.97/kmrcl.asd"
           ;; "thirdparty/lw-compat/_darcs/current/lw-compat.asd"
           ;; "thirdparty/lw-compat/lw-compat.asd"
           ;; "thirdparty/md5/md5.asd"
           ;; "thirdparty/metabang-bind/metabang-bind-test.asd"
           ;; "thirdparty/metabang-bind/metabang-bind.asd"
           ;; "thirdparty/parenscript/parenscript.asd"
           ;; "thirdparty/parse-number/_darcs/pristine/parse-number.asd"
           ;; "thirdparty/parse-number/parse-number.asd"
           ;; "thirdparty/pg/pg.asd"
           ;; "thirdparty/plexippus-xpath/_darcs/pristine/xpath.asd"
           ;; "thirdparty/plexippus-xpath/xpath.asd"
           ;; "thirdparty/puri/puri.asd"
           ;; "thirdparty/rfc2388/rfc2388.asd"
           ;; "thirdparty/rt-20040621/rt.asd"
           ;; "thirdparty/salza-0.7.4/salza.asd"
           ;; "thirdparty/salza-png-1.0.1/salza-png.asd"
           ;; "thirdparty/screamer/screamer.asd"
           ;; "thirdparty/slime/swank.asd"
           ;; "thirdparty/split-sequence/split-sequence.asd"
           ;; "thirdparty/stefil/_darcs/pristine/stefil.asd"
           ;; "thirdparty/stefil/stefil.asd"
           ;; "thirdparty/stem/stem.asd"
           ;; "thirdparty/trivial-backtrace/_darcs/pristine/trivial-backtrace-test.asd"
           ;; "thirdparty/trivial-backtrace/_darcs/pristine/trivial-backtrace.asd"
           ;; "thirdparty/trivial-backtrace/trivial-backtrace-test.asd"
           ;; "thirdparty/trivial-backtrace/trivial-backtrace.asd"
           ;; "thirdparty/trivial-features/_darcs/pristine/trivial-features-tests.asd"
           ;; "thirdparty/trivial-features/_darcs/pristine/trivial-features.asd"
           ;; "thirdparty/trivial-features/trivial-features-tests.asd"
           ;; "thirdparty/trivial-features/trivial-features.asd"
           ;; "thirdparty/trivial-garbage/_darcs/pristine/trivial-garbage.asd"
           ;; "thirdparty/trivial-garbage/trivial-garbage.asd"
           ;; "thirdparty/trivial-gray-streams/trivial-gray-streams.asd"
           ;; "thirdparty/trivial-https/trivial-https.asd"
           ;; "thirdparty/trivial-sockets/trivial-sockets.asd"
           ;; "thirdparty/trivial-utf-8/_darcs/pristine/trivial-utf-8.asd"
           ;; "thirdparty/trivial-utf-8/trivial-utf-8.asd"
           ;; "thirdparty/uffi/uffi-tests.asd"
           ;; "thirdparty/uffi/uffi.asd"
           ;; "thirdparty/unit-test/unit-test.asd"
           ;; "thirdparty/usocket/usocket-test.asd"
           ;; "thirdparty/usocket/usocket.asd"
           ;; "thirdparty/vecto-1.0.2/vecto.asd"
           ;; "thirdparty/xuriella/xuriella.asd"
           ;; "thirdparty/zpb-exif/zpb-exif.asd"
           ;; "thirdparty/zpb-ttf-0.7/zpb-ttf.asd"
           ))
    (bknr-datastore (github "hanshuebner")
     :asd ("src/bknr.xml.asd"
           "src/bknr.impex.asd"
           "src/bknr.data.impex.asd"
           "src/bknr.skip-list.asd"
           "src/bknr.utils.asd"
           "src/bknr.indices.asd"
           "src/bknr.datastore.asd"))
    (bknr-web (github "hanshuebner")
     :asd ("src/bknr.web.asd"
           "src/html-match/html-match.asd"
           "modules/spider/leech.asd"
           "modules/bknr.modules.asd"))
    (black-tie (github "aerique"))
    (blackthorn (google-code hg "blackthorn-engine")
     :asd ("blackthorn.asd"
           "thopter.asd"
           "blackthorn-collision-test.asd"
           "blackthorn-test.asd"
           "bunnyslayer.asd"
           "blackthorn-stress-test.asd"))
    (blackthorn3d (google-code hg "blackthorn-engine-3d")
     :asd ("blank.asd"
           "lkcas.asd"
           "blackthorn3d.asd"
           "blackthorn3d-test.asd"))
    (blocky (github "dto"))
    (blogger (google-code svn "cl-blogger"))
    (blogworks (github "madnificent"))
    (bordeaux-fft (github "ahefner"))
    ;; http://common-lisp.net/project/bordeaux-threads
    (bordeaux-threads (clnet git)
     :asd ("bordeaux-threads.asd"
           "bordeaux-threads-test.asd"))
    (bratwurst (github "sabetts"))
    (buclet (github "aerique"))
    (buildapp (github "xach"))
    ;; Not present on 2011-11-30 ... move code someplace.
    ;; (buildnode (github "bobbysmith007")
    ;;  :asd ("buildnode-excel.asd"
    ;;        "buildnode-xul.asd"
    ;;        "buildnode-kml.asd"
    ;;        "buildnode.asd"
    ;;        "buildnode-xhtml.asd"))
    (bytemap git "http://common-lisp.net/projects/bytemap/bytemap.git"
     :asd ("bytemap-test.asd"
           "bytemap.asd"))
    (c-amplify (github "deplinenoise"))
    (caleb svn "svn://common-lisp.net/project/caleb/svn")
    ;; XXXX: Should work soon.
    ;; (categories git "git://codebasehq.com/bywicket/xg/categories.git"
    (caveman (github "fukamachi")
     :asd ("caveman.asd"
           "caveman-test.asd"
           "skeleton/skeleton.asd"))
    ;; XXXX: switch to checking out the Linux sources ... or maybe everything
    (ccl svn "http://svn.clozure.com/publicsvn/openmcl/trunk/darwinx86/ccl"
     :asd none)
    (cclan (sourceforge cvs)
     :asd ("packages/meta/meta.asd"))
    (cello (clnet cvs)
     :asd ("cello.asd"
           "cellodemo/cellodemo.asd"
           "cffi-extender/cffi-extender.asd"
           "cl-freetype/cl-freetype.asd"
           "cl-freetype/cl-rsrc.asd"
           "cl-ftgl/cl-ftgl.asd"
           ;; "cl-magick/cl-magick.asd"
           ;; "cl-openal/cl-openal.asd"
           "kt-opengl/kt-opengl.asd"))
    (cells (github "Ramarren")
     :asd ("cells-test/cells-test.asd"
           "cells.asd"
           "gui-geometry/gui-geometry.asd"
           "utils-kt.asd"))
    (cells-gtk3 (github "Ramarren")
     :asd ("cells-gtk.asd"
           "gtk-ffi.asd"
           "ph-maths.asd"
           "pod-utils.asd"
           "test-gtk.asd"))
    ;; http://common-lisp.net/project/cffi
    ;; XXXX I saw a mention of cffi repositories on gitorious.
    (cffi (clnet git)
     :asd ("cffi.asd"
           "cffi-examples.asd"
           "cffi-grovel.asd"
           "cffi-tests.asd"
           "cffi-uffi-compat.asd"
           "uffi-compat/uffi.asd"))
    (cffi-j (github "Ramarren"))
    (cffi-redland (github "Ramarren")
     :asd ("redland.asd"
           "sparql-macro.asd"))
    (cffi-stfl (github "Ramarren"))
    (cffi-udp git "http://cl-www.msi.co.jp/projects/cffi-udp/cffi-udp.git")
    (cffi-util darcs "http://common-lisp.net/project/bdb/darcs/cffi-util")
    (cffi-wordnet (github "kraison"))
    (ch-asdf (github "slyrus"))
    ;; ch-image is also available on github, but this version seems newer.
    ;; XXXX: Look at the two again.
    (ch-image (harmon)
     :asd ("ch-image.asd"
           "ch-image-doc.asd"
           "ch-image-test.asd"))
    (ch-util (harmon)
     :asd ("ch-util.asd"
           "ch-util-test.asd"))
    (chanl (github "sykopomp"))
    ;; XXXX: currently broken ... email Pascal J. Bourguignon
    ;; (check-pathnames git "git://git.informatimago.com/public/misc/check-pathnames" :asd none)
    (chemicl (github "slyrus")
     :asd ("chemicl-doc.asd"
           "chemicl-test.asd"
           "chemicl.asd"))
    (chillax (github "sykopomp")
     :asd ("chillax.view-server.asd"
           "chillax.asd"
           "chillax.core.asd"
           "chillax.yason.asd"))
    (chipz (github "froydnj"))
    (chronicity (github "chaitanyagupta")
     :asd ("chronicity.asd"
           "chronicity-test.asd"))
    (chunga (github "edicl"))
    (city-hash (github "brown")
     :asd ("city-hash.asd"
           "city-hash-test.asd"))
    (cl+ssl (gitorious "cl-plus-ssl" "cl-plus-ssl"))
    (cl-2d (github "tpapp"))
    (cl-amazonproduct (github "arielnetworks")
     :asd ("cl-amazonproduct.asd"
           "cl-amazonproduct-test.asd"))
    (cl-annot (github "arielnetworks")
     :asd ("cl-annot.asd"
           "cl-annot-test.asd"))
    (cl-anonfun (github "arielnetworks"))
    (cl-autorepo (github "billstclair"))
    (cl-azure (github "RobBlackwell"))
    (cl-base64 (b9))
    (cl-bayesian (github "tpapp"))
    (cl-beanstalk (github "antifuchs"))
    (cl-bench (clnet svn)
     :asd none)
    (cl-berkeley-db (clnet darcs)
     :asd ("src/cl-berkeley-db.asd"))
    (cl-binary-file-trunk (sourceforge svn "cl-binary-file")
     :asd ("cl-binary-file-trunk.asd"
           "big-endian/big-endian.asd"
           "little-endian/little-endian.asd"))
    (cl-bio (github "slyrus")
     :asd ("cl-bio-align.asd"
           "cl-bio-doc.asd"
           "cl-bio-entrez-doc.asd"
           "cl-bio-entrez-test.asd"
           "cl-bio-entrez.asd"
           "cl-bio-rucksack.asd"
           "cl-bio-taxonomy.asd"
           "cl-bio-test.asd"
           "cl-bio.asd"))
    (cl-bliky (github "fons"))
    (cl-blockfort (github "gonzojive"))
    (cl-blog svn "svn://unmutual.info/cl-blog/trunk/cl-blog")
    (cl-bmp (github "mon-key"))
    (cl-btree (github "gonzojive")
     :asd ("btree/btree.asd"))
    (cl-btree-trunk (sourceforge svn "cl-btree"))
    (cl-buchberger (github "jmbr"))
    (cl-bzip2 (clnet darcs))
    (cl-cairo2 (github "tpapp")
     :asd ("cl-cairo2-xlib.asd"
           "cl-cairo2-quartz.asd"
           "cl-cairo2-win32.asd"
           "cl-cairo2.asd"
           "a-cl-cairo2-loader.asd"))
    (cl-ci (github "billitch"))
    (cl-closure-template (github "archimag")
     :asd ("closure-template.asd"))
    (cl-colors (github "tpapp"))
    (cl-common-blog (github "gihnius"))
    (cl-cont (clnet darcs)
     :asd ("cl-cont.asd"
           "cl-cont-test.asd"))
    (cl-containers darcs "http://common-lisp.net/project/cl-containers/darcs"
     :asd ("cl-containers.asd"
           "cl-containers-documentation.asd"
           "cl-containers-test.asd"))
    (cl-couch (clnet darcs)
     :asd ("cl-couch.asd"
           "cl-couchdb-client.asd"
           "cl-couchdb-object-layer.asd"
           "cl-couchdb-view-server.asd"
           "logv.asd"))
    (cl-crc64 (github "RobBlackwell"))
    ;; Not present on 2011-11-30 ... move code someplace.
    ;; (cl-creditcard (github "bobbysmith007")
    ;;  :asd ("cl-creditcard.asd"
    ;;        "cl-authorize-net.asd"))
    (cl-crypto (github "billstclair"))
    (cl-css (github "madnificent"))
    (cl-curl svn "svn://common-lisp.net/project/cl-curl/subversion/trunk"
     :asd ("curl.asd"))
    (cl-darcs svn "svn://common-lisp.net/project/cl-darcs/svn/cl-darcs/trunk")
    (cl-db-comparison (github "killerstorm")
     :asd none)
    (cl-dbus (github "blitz"))
    (cl-devil (github "sykopomp"))
    (cl-dot svn "http://svn.foldr.org/~michaelw/cl-dot/trunk")
    (cl-dwarf (gitorious)
     :asd none)

    ;; Checking out this repository prints:
    ;; This repo is OBSOLETE!
    ;; The new darcs2 repo is available at http://dwim.hu
    ;; XXXX: remove this repository and cl-dwim-old
    ;; (cl-dwim (clnet darcs)
    ;;  :asd ("dwim.asd" "dwim-meta-model-test.asd"))

    (cl-elf (repo-or-cz))
    (cl-env (github "franzinc"))
    (cl-eshop (github "rigidus")
     :asd none)
    (cl-fad (github "edicl"))
    (cl-fidelia (github "billstclair"))
    (cl-fluiddb (github "hdurer")
     :asd ("cl-fluiddb.asd"
           "cl-fluiddb-test.asd"))
    ;; Repository has disappeared.  User still there.
    ;; (cl-frame (github "dto")
    ;;  :asd none)
    (cl-freeswitch (github "kraison"))
    (cl-future (github "jpalmucci"))
    (cl-gambol (google-code svn)
     :asd ("gambol.asd"))
    (cl-gcalc (google-code svn))
    (cl-gd (github "edicl")
     :asd ("cl-gd-test.asd"
           "cl-gd.asd"))
    (cl-genomic (github "keithj")
     :asd ("cl-genomic.asd"
           "cl-genomic-test.asd"))
    (cl-geometry (github "Ramarren")
     :asd ("cl-geometry.asd"
           "cl-geometry-tests.asd"))
    (cl-geonames (google-code svn)
     :asd ("cl-geonames.asd"
           "cl-geonames-test.asd"))
    (cl-gordon (sourceforge svn)
     :asd ("gordon/gordon.asd"
           ;; "gordon-branches/gordon-clos/gordon.asd"
           "torta/torta.asd"
           "ttf-flash/ttf-flash.asd"))
    (cl-gpu (github "angavrilov")
     :asd ("cl-gpu.core.asd"
           "cl-gpu.test.asd"
           "cl-gpu.cuda.asd"
           "cl-gpu.asd"
           "cl-gpu.buffers.asd"))
    (cl-graph darcs "http://common-lisp.net/project/cl-graph"
     :asd ("cl-graph.asd"
           "cl-graph-test.asd"))
    ;; Primary repository http://git.nklein.com/lisp/libs/cl-growl.git is
    ;; unreadable.
    (cl-growl (github "nklein"))
    (cl-gtk2 (repo-or-cz)
     :asd ("cairo/cl-gtk2-cairo.asd"
           "gdk/cl-gtk2-gdk.asd"
           "glib/cl-gtk2-glib.asd"
           "gtk-glext/cl-gtk2-gtkglext.asd"
           "gtk/cl-gtk2-gtk.asd"
           "pango/cl-gtk2-pango.asd"))
    (cl-hmm (google-code svn))
    ;; XXXX: Not sure if this one is the best.
    (cl-i18n (bitbucket "skypher"))
    (cl-imagemagick (github "franzinc")
     :asd none)
    ;; Not present on 2011-11-30 ... move code someplace.
    ;; (cl-inflector (github "bobbysmith007"))
    (cl-interpol (github "edicl"))
    (cl-irc (clnet svn)
     :asd ("cl-irc.asd"
           "example/cliki-bot.asd"
           "test/cl-irc-test.asd"))
    (cl-irregsexp (clnet git)
     :asd ("cl-irregsexp-test.asd"
           "cl-irregsexp.asd"))
    (cl-jags (github "tpapp")
     :asd ("cl-jags.asd"
           "cl-jags-tests.asd"))
    ;; Trouble updating from repository on 2011-09-17.
    ;; Trouble updating from repository on 2011-10-26.
    ;; Trouble updating from repository on 2011-11-30.
    (cl-jpeg (clnet cvs "cljl"))
    (cl-js (github "akapav" "js"))
    (cl-json (clnet darcs))
    (cl-kyoto-cabinet (github "kraison"))
    (cl-l10n (clnet darcs))
    (cl-lastfm (github "nlamirault")
     :asd ("cl-lastfm.asd"
           "cl-lastfm-test.asd"))
    (cl-lex (google-code svn))
    (cl-librarian darcs "http://www.pasternacki.net/repos/cl-librarian"
     :asd ("cl-librarian.asd"
           "skel/skel.asd"))
    (cl-libsvm (melis))
    (cl-libtai (clnet cvs))
    (cl-libxml2 (github "archimag")
     :asd ("xfactory.asd"
           "cl-libxslt.asd"
           "cl-libxml2.asd"
           "xoverlay.asd"))
    (cl-llvm (repo-or-cz))
    (cl-locale (github "arielnetworks")
     :asd ("cl-locale.asd"
           "cl-locale-test.asd"))
    (cl-loom (github "billstclair"))
    ;; Repository missing on 2011-09-17.
    ;; Repository missing on 2011-10-26.
    ;; Repository missing on 2011-11-30.
    (cl-loop-plus (github "arielnetworks"))
    (cl-magick (clnet cvs))
    (cl-markdown darcs "http://common-lisp.net/project/cl-markdown"
     :asd ("cl-markdown-comparisons.asd"
           "cl-markdown-test.asd"
           "cl-markdown.asd"))
    (cl-markup (github "arielnetworks")
     :asd ("cl-markup.asd"
           "cl-markup-test.asd"))
    (cl-mathstats darcs "http://common-lisp.net/project/cl-mathstats"
     :asd ("cl-mathstats.asd"
           "cl-mathstats-test.asd"))
    ;; Not present on 2011-11-30 ... move code someplace.
    ;; (cl-mediawiki (github "bobbysmith007"))
    (cl-memcached (github "arielnetworks"))
    (cl-menusystem (clnet cvs))
    (cl-migrations (clnet darcs))
    (cl-mill (google-code svn)
     :asd ("gcode.asd"))
    (cl-monad-macros (clnet svn))
    (cl-moneris (github "vsedach")
     :asd ("cl-moneris.asd"
           "cl-moneris-test.asd"))
    (cl-money-type (google-code hg))
    (cl-mongo (github "fons"))
    (cl-mongrel2 (github "vseloved"))

    ;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ;;    (mpeg (clnet cvs "cl-mp3-parse" "cl-mp3-parse") :asd none)

    (cl-mpi (google-code svn)
     :asd ("cl-mpi.asd"
           "par-eval.asd"))
    (cl-mssql (github "archimag")
     :asd ("mssql.asd"))
    (cl-mw git "http://pages.cs.wisc.edu/~psilord/lisp-public/public-repos-lisp/cl-mw.git"
     :asd ("cl-mw.examples.higher-order.asd"
           "cl-mw.asd"
           "cl-mw.examples.monte-carlo-pi.asd"
           "cl-mw.examples.ping.asd"
           "cl-mw.examples.hello-world.asd"))
    (cl-mysql (github "hackinghat")
     :asd ("cl-mysql-test.asd"
           "cl-mysql.asd"))
    (cl-n-back (github "smithzvk"))
    (cl-ncurses (clnet svn))
    (cl-neo4j (github "kraison"))
    (cl-net-snmp (sourceforge svn)
     :asd ("asn.1/trunk/asn.1-dev.asd"
           "asn.1/trunk/asn.1.asd"
           "contrib/msi/cffi-udp/cffi-udp.asd"
           "contrib/msi/snmp-nonblocking/snmp-nonblocking.asd"
           "ipmi/trunk/ipmi.asd"
           "ldap/trunk/ldap.asd"
           "ldap/trunk/trivial-ldap-0.71/trivial-ldap.asd"
           "lispworks-udp/trunk/lispworks-udp.asd"
           ;; "snmp/trunk/snmp-mib.asd"
           "snmp/trunk/snmp-server.asd"
           "snmp/trunk/snmp-test.asd"
           "snmp/trunk/snmp-ui.asd"
           "snmp/trunk/snmp.asd"
           ;; This is the canonical version of usocket-udp.
           "usocket-udp/trunk/usocket-udp.asd"))
    ;; Repository problem on 2011-11-30.
    (cl-notify (repo-or-cz))
    (cl-num-utils (github "tpapp"))
    (cl-numlib (github "tpapp"))
    (cl-oauth (github "skypher"))
    (cl-objc (clnet darcs))
    (cl-observer (google-code svn))
    (cl-openal (github "sykopomp")
     :asd ("cl-openal.asd"
           "cl-openal-examples.asd"))
    (cl-openbox (github "stassats"))
    (cl-opencalais (github "RobBlackwell"))
    ;; XXXXXXXXXX: Compare this to the cl-opengl below and delete one.
    (cl-opengl (github "3b")
     :asd ("cl-glu.asd"
           "cl-glut-examples.asd"
           "cl-glut.asd"
           "cl-opengl.asd"))
    ;; (cl-opengl (clnet darcs)
    ;;  :asd ("cl-glu.asd"
    ;;        "cl-glut-examples.asd"
    ;;        "cl-glut.asd"
    ;;        "cl-opengl.asd"))
    (cl-openid (clnet darcs))
    (cl-pack (github "dballard")
     :asd ("cl-pack.asd"
           "ieee-floats/ieee-floats.asd"))
    (cl-parsec (github "vseloved"))
    (cl-parser-combinators (github "Ramarren")
     :asd ("parser-combinators.asd"
           "parser-combinators-tests.asd"))
    (cl-pattern (github "arielnetworks")
     :asd ("cl-pattern.asd"
           "cl-pattern-test.asd"
           "cl-pattern-benchmark.asd"))
    (cl-pdf svn "http://www.fractalconcept.com:8000/public/open-source/cl-pdf"
     :asd ("cl-pdf.asd"
           "cl-pdf-parser.asd"
           "salza/salza.asd"))
    (cl-pdf-jp (github "quek"))
    (cl-peg darcs "http://subvert-the-dominant-paradigm.net/repos/cl-peg")
    (cl-period (google-code svn))
    (cl-photo (b9)
     :asd ("cl-photo.asd"
           "cl-photo-tests.asd"))
    (cl-ppcre (github "edicl")
     :asd ("cl-ppcre-unicode.asd"
           "cl-ppcre.asd"))
    ;; The web page says the repository on bitbucket is the most recent.
    ;; My old version is from:  (cl-prevalence (clnet cvs))
    (cl-prevalence (bitbucket "skypher")
     :asd ("cl-prevalence.asd"
           "test/cl-prevalence-test.asd"))
    (cl-prolog (github "keithj")
     :asd ("cl-prolog.asd"
           "cl-prolog-test.asd"
           "cl-swi.asd"
           "cl-swi-client.asd"))
    (cl-qt-web-browser (github "stassats"))
    (cl-randist (github "lvaruzza"))
    (cl-random (github "tpapp"))
    (cl-rdfxml svn "http://svn.cs.rpi.edu/svn/tayloj/cl-rdfxml")
    (cl-recaptcha (github "madnificent"))
    ;; XXXX: This version of cl-rdfxml may be better.  Active development?
    ;; (cl-rdfxml (github "turbo24prg") :asd none)
    (cl-redis (github "vseloved"))
    (cl-rogue (google-code svn))
    (cl-ruby (google-code hg)
     :asd none)
    (cl-sails (github "gonzojive"))
    (cl-sam (github "keithj")
     :asd ("cl-sam.asd"
           "cl-sam-test.asd"))
    (cl-selenium (clnet cvs)
     :asd ("selenium.asd"))
    (cl-simd (github "angavrilov"))
    (cl-skip-list (github "kraison"))
    (cl-skunk (github "fons"))
    (cl-slog (bitbucket "skypher"))
    (cl-smtp (clnet cvs))
    (cl-spidermonkey (github "gonzojive"))
    (cl-sqlite (repo-or-cz)
     :asd ("sqlite-tests.asd"
           "sqlite.asd"))
    (cl-starcraft-proxybot (github "aerique"))
    (cl-stm darcs "http://common-lisp.net/project/cl-stm")
    (cl-stomp (clnet git)
     :asd ("cl-stomp.asd"
           "cl-stomp-example.asd"))
    (cl-store (clnet cvs))
    (cl-strings (google-code svn)
     :asd ("cl-strings.asd"
           "cl-strings-tests.asd"))
    (cl-stripe (github "antifuchs"))
    (cl-svg (google-code svn))
    ;; http://common-lisp.net/projects/cl-machine-learning/git/cl-svm/.git
    ;; Look for a newer cl-swm repository at github.com/gonzojive.
    (cl-svm git "http://common-lisp.net/project/suave/git/cl-svm/.git")
    (cl-swap-file-trunk (sourceforge svn "cl-swap-file"))
    (cl-taint darcs "http://www.common-lisp.net/project/cl-taint/cl-taint-release")
    (cl-tc (github "unya")
     :asd ("cl-tc.asd"
           "tokyocabinet.asd"
           "tokyodystopia.asd"
           "tokyotyrant.asd"))
    (cl-telnetd (clnet cvs)
     :asd none)
    (cl-test-more (github "fukamachi"))
    (cl-tetris3d (github "grouzen"))
    (cl-text-tables (github "tpapp")
     :asd ("cl-text-tables.asd"
           "cl-text-tables-tests.asd"))
    (cl-tidy (github "gonzojive"))
    (cl-tk (github "marijnh"))
    (cl-tokyo-cabinet (github "keithj")
     :asd ("cl-tokyo-cabinet.asd"
           "cl-tokyo-cabinet-test.asd"))
    (cl-transactional (github "Ramarren")
     :asd ("cl-transactional-tests.asd"
           "cl-transactional.asd"))
    (cl-tuples (repo-or-cz)
     :asd ("cl-tuples.asd"
           "cl-tuples-test.asd"))
    (cl-twit (github "chaitanyagupta"))
    (cl-twitter (clnet darcs)
     :asd ("cl-twitter.asd"
           "cl-twitter-db.asd"))
    (cl-typesetting svn "http://www.fractalconcept.com:8000/public/open-source/cl-typesetting"
     :asd ("cl-typegraph.asd"
           "cl-typesetting.asd"
           "cl-typesetting-test.asd"
           "contrib/xhtml-renderer/xml-render.asd"
           "documentation/lisp-source/cl-pdf-doc.asd"))
    (cl-unicode (github "edicl"))
    (cl-unification (clnet cvs)
     :asd ("cl-unification.asd"
           "cl-unification-lib.asd"))
    ;; Version maintained by pix@kepibu.org, who posts to the developer
    ;; mailing list as pinterface@gmail.com
    (cl-unification-pinterface darcs "http://repo.kepibu.org/cl-unification"
     :asd none)
    (cl-uri (clnet darcs)
     :asd ("src/cl-uri.asd"))
    (cl-uri-templates (github "billitch")
     :asd ("cl-uri-templates.test.asd"
           "cl-uri-templates.asd"))
    (cl-utilities (clnet cvs))
    (cl-variates (clnet darcs)
     :asd ("cl-variates.asd"
           "cl-variates-test.asd"))
    (cl-vh (github "quek"))
    (cl-wal-trunk (sourceforge svn "cl-wal"))
    (cl-web-crawler (google-code svn))
    (cl-webdav (github "edicl"))
    (cl-who (github "edicl"))
    (cl-whois (github "billitch"))
    (cl-win32ole (github "quek")
     :asd ("cl-win32ole.asd"
           "cl-win32ole-sys.asd"))
    (cl-x86-asm (repo-or-cz))
    (cl-xspf (google-code svn))
    (cl-xmpp (clnet cvs)
     :asd ("cl-xmpp-sasl.asd"
           "cl-xmpp-tls.asd"
           "cl-xmpp.asd"
           "test/cl-xmpp-test.asd"))
    (cl-xsands (github "quek"))
    (cl-zipper (github "danielfm"))
    ;; XXXX: rename to zeromq
    (cl-zmq (repo-or-cz)
     :asd ("zeromq.asd"))
    (clack (github "fukamachi")
     :asd ("clack.asd"
           "clack-test.asd"))
    (clack-doc (github "fukamachi"))
    (clans (github "patzy"))
    (clawk (github "sharplispers"))
    (claymore (github "madnificent" "cl-aymore")
     :asd ("claymore.asd"
           "tests/claymore.tests.asd"))
    (claw (clnet svn)
     :asd ("main/claw-as/claw-as.asd"
           "main/claw-demo/claw-demo.asd"
           "main/claw-html.dojo/claw-html.dojo.asd"
           "main/claw-html/claw-html.asd"
           "main/claw.i18n/claw.i18n.asd"
           "main/connectors/claw-hunchentoot-connector/claw-hunchentoot-connector.asd"
           "site/claw-site.asd"))
    (clayworks (github "madnificent"))
    (clazy (clnet cvs))
    ;; May no longer be maintained ... appears to be gone on gitorious.
    ;; (clbuild (gitorious)
    ;;  :asd none)
    (clbuild2 (gitorious)
     :asd none)
    (clee (github "fukamachi"))
    (clem (github "slyrus")
     :asd ("clem.asd"
           "clem-benchmark.asd"
           "clem-doc.asd"
           "clem-test.asd"))
    (cletris (github "nlamirault")
     :asd ("cletris.asd"
           "cletris-network.asd"))
    (clews git "http://www.jarw.org.uk/lisp/clews.git"
     :asd ("peer-review/clews.peer-review.asd"
           "assessment/clews.assessment.discussion.asd"
           "assessment/clews.assessment.socratic.asd"
           "assessment/clews.assessment.asd"
           "clews.asd"
           "discussion/clews.discussion.asd"
           "form/clews.form.asd"
           "articles/clews.articles.asd"))
    (clg (sourceforge cvs)
     :asd ("atk/atk.asd"
           "cairo/cairo.asd"
           "gdk/gdk.asd"
           "gffi/gffi.asd"
           "gio/gio.asd"
           "glade-xml/glade-xml.asd"
           "glib/glib.asd"
           "gtk/gtk.asd"
           "pango/pango.asd"
           "rsvg/rsvg.asd"
           "tools/clg-tools.asd"))
    (clim-chess (github "stassats"))
    (clim-desktop (clnet cvs)
     :asd ("clim-desktop.asd"
           "clim-desktop-minimal.asd"))
    (climacs (clnet cvs))
    (climc (google-code svn))
    (clime (github "mon-key"))
    (climon (github "nlamirault"))
    (clisp hg "http://clisp.hg.sourceforge.net:8000/hgroot/clisp/clisp"
     :asd none)

    ;; XXXX: figure out where this is
    ;;    (clnuplot darcs "http://common-lisp.net/project/clnuplot")

    (clixdoc (github "hanshuebner"))
    (cloak git "http://www.lichteblau.com/git/cloakbuild.git"
     :asd ("cloak/cloak.asd"
           "java-cloak-compat/java-cloak-compat.asd"
           "sb-regpair/sb-regpair.asd"))
    ;; blindglobe has a clocc repository on github.
    (clocc (sourceforge cvs)
     :asd ("src/f2cl/debian/f2cl.asd"
           "src/f2cl/f2cl.asd"
           "src/f2cl/packages/fishpack.asd"
           "src/games/cil/cil.asd"
           "src/gui/clue/clio.asd"
           "src/gui/clue/clue.asd"
           "src/gui/clue/pictures.asd"
           ;; XXXX: What CLX do we choose?
           ;; "src/gui/clx/clx-vm/clx-vm.asd"
           ;; "src/gui/clx/clx.asd"
           "src/tools/memoization/memoization.asd"))
    (clois-lane (github "aerique")
     :asd ("clois-lane.asd"
           "clois-lane-cegui.asd"))
    (clommand (github "smithzvk"))
    ;; Gabor Melis' task scheduler.
    (clon-task-scheduler (melis "clon")
     :asd none)
    ;; David O'Toole's prototype object system.
    ;; Currently missing from github.  XXXX: Move source?
    ;; (clon-prototype-objects (github "dto" "clon") :asd none)
    ;; Didier Verna's command line options nuker.
    (clon-command-line git "http://www.lrde.epita.fr/~didier/software/lisp/clon/clon.git"
     :asd none)
    (clonsigna (clnet git))
    (closer-mop darcs "http://common-lisp.net/project/closer/repos/closer-mop")
    (closure (clnet cvs))
    ;; Repository problem on 2011-11-30.
    (closure-common (repo-or-cz))
    (closure-html (repo-or-cz))
    (clouchdb (clnet cvs)
     :asd ("src/clouchdb.asd"
           "src/clouchdb-examples.asd"
           "src/clouchdb-tests.asd"))
    (cloze-call (google-code svn)
     :asd none)
    (clpython (github "franzinc" "cl-python"))
    (clqr (github "trebb")
     :asd none)
    (clrs (github "willijar"))
    (clsem (github "antifuchs"))
    (clsql (b9)
     :asd ("clsql.asd"
           "clsql-aodbc.asd"
           "clsql-db2.asd"
           "clsql-mysql.asd"
           "clsql-odbc.asd"
           "clsql-oracle.asd"
           "clsql-postgresql-socket.asd"
           "clsql-postgresql.asd"
           "clsql-sqlite.asd"
           "clsql-sqlite3.asd"
           "clsql-tests.asd"
           "clsql-uffi.asd"))
    ;; Not present on 2011-11-30 ... move code someplace.
    ;; (clsql-orm (github "bobbysmith007"))
    (clsr (harmon)
     :asd ("clsr.asd"
           "clsr-doc.asd"
           "clsr-gen.asd"
           "clsr-test.asd"))
    (cluck (b9))
    (clucumber (github "antifuchs")
     :asd ("lib/clucumber/clucumber.asd"))
    (cluster-ffi (harmon)
     :asd ("cluster-ffi.asd"
           "cluster-ffi-gen.asd"))
    (clws (github "3b"))
    (clx darcs "http://common-lisp.net/~crhodes/clx")
    (clx-franz (github "franzinc" "clx")
     :asd none)
    (clysma (github "aerique")
     :asd ("clysma.asd"
           "clysma-gtk.asd"))
    (cm-openid (github "madnificent"))
    (cmlisp1 (github "g000001"))
    (cmucl (clnet cvs "cmucl" "src")
     :asd none)
    ;; Not present on 2011-11-30 ... move code someplace.
    ;; (collectors (github "bobbysmith007"))
    (com.dvlsoft.declt git "http://www.lrde.epita.fr/~didier/software/lisp/declt/declt.git")
    (com.gigamonkeys.binary-data (github "gigamonkey" "monkeylib-binary-data"))
    (com.gigamonkeys.foo (github "gigamonkey" "monkeylib-foo"))
    (com.gigamonkeys.id3v2 (github "slyrus" "monkeylib-id3")
     :asd ("com.gigamonkeys.id3v2.asd"))
    (com.gigamonkeys.json (github "gigamonkey" "monkeylib-json"))
    (com.gigamonkeys.macro-utilities (github "gigamonkey" "monkeylib-macro-utilities"))
    (com.gigamonkeys.markup (github "gigamonkey" "monkeylib-markup"))
    (com.gigamonkeys.parser (github "gigamonkey" "monkeylib-parser"))
    (com.gigamonkeys.pathnames (github "gigamonkey" "monkeylib-pathnames"))
    (com.gigamonkeys.prose-diff (github "gigamonkey" "monkeylib-prose-diff"))
    (com.gigamonkeys.spam (github "gigamonkey" "monkeylib-spam"))
    (com.gigamonkeys.statistics (github "gigamonkey" "monkeylib-statistics"))
    (com.gigamonkeys.test-framework (github "gigamonkey" "monkeylib-test-framework"))
    (com.gigamonkeys.utilities (github "gigamonkey" "monkeylib-utilities"))
    (com.google.base (github "brown" "base"))
    (com.google.flag (github "brown" "lisp-gflags")
     :asd ("com.google.flag.asd"
           "com.google.flag-test.asd"))
    (com.nklein.gl-springs (github "nklein" "com-nklein-gl-springs"))
    (com.nklein.misc (github "nklein" "com-nklein-misc") :asd none)
    (com.nklein.parser-generator (github "nklein")
     :asd ("com.nklein.parser-generator.asd"
           "com.nklein.parser-generator.reader.asd"
           "com.nklein.parser-generator.types.asd"))
    (com.nklein.util.general (github "nklein" "com-nklein-util-general"))
    (com.nklein.util.vec-math  (github "nklein" "com-nklein-util-vec-math"))
    (command-line-arguments (clnet git "qitab"))
    (common-lisp-stat (github "blindglobe")
     :submodules t
     :asd none)
    (common-worm (github "sykopomp"))
    (commonorbit (github "g000001" "CommonORBIT")
     :asd ("CommonORBIT.asd"))
    ;; commonqt-OLD comes from repo.or.cz
    ;; (commonqt git "git://repo.or.cz/commonqt.git" ...)
    ;; Checkout of that repository fails, so I switched to lichteblau's
    ;; XXXX: Which is the canonical source?
    (commonqt git "http://www.lichteblau.com/git/commonqt.git"
     :asd ("qt.asd"
           "qt-repl.asd"
           "qt-tutorial.asd"))
    ;; A fork of the Swank backend that leverages ASDF, bordeaux-threads,
    ;; closer-mop, trivial-gray-streams, usocket, etc.
    (conium (gitorious))
    (consix (github "death"))
    (constantia (github "death"))
    (contextl darcs "http://common-lisp.net/project/closer/repos/contextl")
    (css-lite (github "paddymul"))
    ;; Not present on 2011-11-30 ... move code someplace.
    ;; (css-selectors (github "bobbysmith007"))
    ;; Not present on 2011-11-30 ... move code someplace.
    ;; (csv-parser (github "nikodemus"))
    (cusp svn "http://cusp.googlecode.com/svn" ; missing /trunk on URI
     :asd none)
    (cxml (repo-or-cz))
    (cxml-rpc (github "antifuchs"))
    (data-format-validation (github "willijar" "cl-data-format-validation"))
    (database-migrations (github "madnificent"))
    (date-utils (github "vseloved")
     :asd none)
    (dbus (github "death"))
    (decimal-floats (github "gugamilare")
     :asd ("decimal-floats.asd"
           "decimal-floats-tests.asd"))
    (decimals (github "tlikonen" "cl-decimals"))
    (deflate (github "pmai" "Deflate"))
    (defservice (github "marijnh")
     :asd ("defservice.asd"
           "defservice.aserve.asd"))
    (defsystem-compatibility
     darcs "http://common-lisp.net/project/cl-containers/defsystem-compatibility"
     :asd ("defsystem-compatibility.asd"
           "defsystem-compatibility-test.asd"))
    (demacs (github "vy"))
    (deoxybyte-gzip (github "keithj")
     :asd ("deoxybyte-gzip.asd"
           "deoxybyte-gzip-test.asd"))
    (deoxybyte-io (github "keithj")
     :asd ("deoxybyte-io.asd"
           "deoxybyte-io-test.asd"))
    (deoxybyte-run (github "keithj")
     :asd ("deoxybyte-run.asd"
           "deoxybyte-run-test.asd"))
    (deoxybyte-systems (github "keithj"))
    (deoxybyte-unix (github "keithj")
     :asd ("deoxybyte-unix.asd"
           "deoxybyte-unix-test.asd"))
    (deoxybyte-utilities (github "keithj")
     :asd ("deoxybyte-utilities.asd"
           "deoxybyte-utilities-test.asd"))
    (detachtty darcs "http://common-lisp.net/project/bese/repos/detachtty"
     :asd none)
    (dictionary git "http://www.jarw.org.uk/lisp/cl-dictionary.git"
     :asd ("sql-dictionary.asd"
           "db-dictionary.asd"
           "dictionary.asd"))
    (discworld (github "cddr"))
    (dispatch (github "sellout" "cl-dispatch"))
    (docudown darcs "http://common-lisp.net/project/docudown"
     :asd ("docudown.asd"
           "docudown-test.asd"))
    (docutils (github "willijar" "cl-docutils")
     :asd ("docutils.asd"
           "extensions/docutils.extensions.asd"))
    (doors (github "Lovesan")
     :asd ("doors.asd"
           "doors.examples.asd"))
    (drakma (github "edicl"))
    (dynamic-classes darcs "http://common-lisp.net/project/dynamic-classes"
     :asd ("dynamic-classes.asd"
           "dynamic-classes-test.asd"))
    (e-on-cl svn "http://switchb.org/svn/e/cl-e/trunk/")
    (eager-future (github "vsedach" "Eager-Future"))
    (eager-future2 (github "vsedach" "Eager-Future2")
     :asd ("eager-future2.asd"
           "test.eager-future2.asd"))
    (ec2 (github "nikodemus"))
    ;; XXXX: The git repositories don't seem to have the latest code.  Try:
    ;; git clone git://ecls.git.sourceforge.net/gitroot/ecls/ecls
    (ecl git "http://ecls.sourceforge.net/git/ecl/.git"
     :asd none)
    (ecl-doc git "http://ecls.sourceforge.net/git/ecl-doc/.git"
     :asd none)
    (ecl-test git "http://ecls.sourceforge.net/git/ecl-test/.git"
     :asd none)
    (eclipse (clnet cvs)
     :asd none)
    (elephant (clnet darcs "elephant" "elephant-1.0")
     :asd ("ele-bdb.asd"
           "ele-clp.asd"
           "ele-postmodern.asd"
           "elephant-tests.asd"
           "elephant.asd"
           "src/contrib/eslick/db-acache/ele-acache.asd"
           "src/contrib/eslick/db-lisp/ele-lisp.asd"
           "src/contrib/eslick/db-prevalence/ele-prevalence.asd"
           "src/contrib/rread/db-clsql/ele-clsql.asd"
           "src/contrib/rread/db-clsql/ele-postgresql.asd"
           "src/contrib/rread/db-clsql/ele-sqlite3.asd"
           "src/contrib/rread/dcm/dcm.asd"))
    (elf git "git://adaptive.cs.unm.edu/elf.git")
    (enumerations cvs pserver anonymous t common-lisp.net "/project/cl-enumeration/cvsroot")
    (eos (github "adlai" "Eos"))
    (epigraph (github "slyrus")
     :asd ("epigraph.asd"
           "epigraph-doc.asd"
           "epigraph-test.asd"))
    (ernestine (google-code svn)
     :asd ("ernestine-gui.asd"
           "ernestine-web.asd"
           "ernestine-tests.asd"
           "ernestine.asd"))
    (esa cvs pserver anonymous t common-lisp.net "/project/climacs/cvsroot")
    (esrap (github "nikodemus"))
    ;; XXXX: Maybe just check out the trunk?  There are tons of tags
    ;; directories.
    (exp-engine svn "http://exp-engine.svn.sourceforge.net/svnroot/exp-engine"
     :asd ("expresso/trunk/clim/expresso-clim.asd"
           "expresso/trunk/x/mapping-engine.asd"
           "expresso/trunk/p21/p21.asd"
           "expresso/trunk/shell/osicat/osicat.asd"
           "expresso/trunk/shell/terminfo/terminfo.asd"
           "expresso/trunk/shell/linedit/linedit.asd"
           "expresso/trunk/shell/expresso-shell.asd"
           ;; "expresso/trunk/shell/uffi/uffi.asd"
           ;; "expresso/trunk/shell/uffi/uffi-tests.asd"
           "expresso/trunk/p11/p11.asd"
           "expresso/trunk/compiler/expresso-compiler.asd"
           "expresso/trunk/expcore/expresso-core.asd"
           "expresso/trunk/expcore/expresso-base.asd"
           "expresso/trunk/expcore/expresso.asd"
           "expresso/trunk/gui/expresso-capi.asd"
           "expresso/trunk/p14/p14.asd"))
    (fare-utils git "git://common-lisp.net/users/frideau/fare-utils.git"
     :asd ("fare-utils.asd"
           "test/fare-utils-test.asd"))
    (faslpath (google-code svn)
     :asd none)
    (femlisp (savannah cvs))
    (ffa (github "tpapp"))
    ;; This used to be cl-fft on http://git.nklein.com/lisp/libs/fft.git
    (fft (github "nklein" "FFT")
     :asd ("fft.asd"
           "pfft.asd"))
    (fftw-bindings (github "smithzvk"))
    (filtered-functions darcs "http://common-lisp.net/project/closer/repos/filtered-functions")
    (finebrush (github "Valera"))
    (fink (github "dballard")
     :asd none)
    (firehose (github "xach"))
    (fiveam darcs "http://common-lisp.net/project/bese/repos/fiveam")
    ;; Unofficial darcs mirrir.
    (flexi-streams darcs "http://common-lisp.net/~loliveira/ediware/flexi-streams"
     :asd none)
    (flexichain (clnet cvs)
     :asd ("flexichain-doc.asd"
           "flexichain-test.asd"
           "flexichain.asd"))
    (flow (github "madnificent"))

    ;; Repository missing on 2010-11-09.
    ;; (folio git "git://codebasehq.com/bywicket/xg/folio.git"
    ;;  :asd ("as/as.asd"
    ;;        "collections/collections.asd"
    ;;        "lib/misc-extensions_1.2.0/misc-extensions.asd"
    ;;        ;; XXXX: Conflict with fset project.
    ;;        ;; "lib/fset_1.2.2/fset.asd"
    ;;        "folio.asd"))

    (format-time (github "xach"))
    (freeimage (github "BradWBeer" "CL-FreeImage"))
    (fricas (sourceforge svn)
     :asd none)
    (fridge (github "madnificent"))
    (fset (clnet svn))
    (fsbv (repo-or-cz))
    (fsdb (github "nikodemus"))
    (fsvd (melis))
    (ftd (clnet darcs))
    (fukacl (github "fukamachi"))
    (function-namespace (github "madnificent"))
    (g000001 (github "g000001"))
    (garbage-pools (google-code svn)
     :asd ("garbage-pools.asd"
           "garbage-pools-test.asd"))
    (garnet cvs pserver anonymous nil garnetlisp.cvs.sourceforge.net "/cvsroot/garnetlisp"
     :asd none)
    (gcl (savannah cvs)
     :asd none)
    (gcc-xml-ffi (harmon)
     :asd ("gcc-xml-ffi.asd"
           "gcc-xml-ffi-test.asd"))
    (geometry (github "xach"))
    (getopt (b9))
    (gigamonkey-distcompiler (github "gigamonkey" "quicklisp-distcompiler"))
    (glaw (github "patzy")
     :asd ("glaw-sdl.asd"
           "glaw-imago.asd"
           "glaw.asd"
           "glaw-examples.asd"))
    (glitter (github "froydnj"))
    (glop (github "patzy")
     :asd ("glop.asd"
           "glop-test.asd"))
    (gotanda (github "fukamachi")
     :asd ("gotanda.asd"
           "gotanda-server.asd"))
    (graph-utils (github "kraison"))
    (gridlock (github "xach"))
    ;; Not present on 2011-11-30 ... move code someplace.
    ;; (group-by (github "bobbysmith007"))
    (grout (github "xach"))
    (gsharp git "git://common-lisp.net/projects/gsharp/gsharp.git")
    (gsd (repo-or-cz)
     :asd ("foreign-array/foreign-array.asd"
           "foreign-array/foreign-array-tests.asd"
           "grid-tests.asd"
           "grid/grid-tests.asd"
           "grid/grid.asd"
           "foreign-array.asd"
           "grid.asd"
           "foreign-array-tests.asd"))
    (gsll (repo-or-cz))
    (gtk (github "franzinc")
     :asd none)
    (hemlock (gitorious)
     :asd ("hemlock.base.asd"
           "hemlock.qt.asd"
           "hemlock.clx.asd"
           "hemlock.tty.asd"))
    ;; The hob repository is also on github, but updates fail from that one.
    (hob git "http://marijn.haverbeke.nl/git/hob")
    (html-entities (google-code svn))
    ;; Not present in bknr/third_party.
    (html-template darcs "http://common-lisp.net/~loliveira/ediware/html-template")
    (http (github "stassats"))
    (http-dohc (github "vsedach" "HTTP-DOHC"))
    ;; XXXXXXXXXXXXXXXXXXXX look for other dwim.hu repositories
    ;; XXXX: finish conversion to dwim-hu
    (hu.dwim.asdf (dwim-hu)
     :asd ("hu.dwim.asdf.documentation.asd"
           "hu.dwim.asdf.asd"))
    (hu.dwim.blog (dwim-hu)
     :asd ("hu.dwim.blog.documentation.asd"
           "hu.dwim.blog.asd"
           "hu.dwim.blog.test.asd"))
    (hu.dwim.brainfuck (dwim-hu)
     :asd ("hu.dwim.brainfuck.test.asd"
           "hu.dwim.brainfuck.asd"))
    (hu.dwim.build (dwim-hu)
     :asd ("hu.dwim.build.documentation.asd"
           "hu.dwim.build.asd"))
    (hu.dwim.common (dwim-hu)
     :asd ("hu.dwim.common.documentation.asd"
           "hu.dwim.common.asd"))
    (hu.dwim.common-lisp (dwim-hu)
     :asd ("hu.dwim.common-lisp.asd"
           "hu.dwim.common-lisp.documentation.asd"))
    (hu.dwim.computed-class (dwim-hu)
     :asd ("hu.dwim.computed-class.asd"
           "hu.dwim.computed-class+swank.asd"
           "hu.dwim.computed-class.documentation.asd"
           "hu.dwim.computed-class+hu.dwim.defclass-star.asd"
           "hu.dwim.computed-class.test.asd"))
    (hu.dwim.debug (dwim-hu)
     :asd ("hu.dwim.debug.documentation.asd"
           "hu.dwim.debug.asd"
           "hu.dwim.debug.test.asd"))
    (hu.dwim.def (dwim-hu)
     :asd ("hu.dwim.def+cl-l10n.asd"
           "hu.dwim.def.namespace.asd"
           "hu.dwim.def+hu.dwim.common.asd"
           "hu.dwim.def.documentation.asd"
           "hu.dwim.def+contextl.asd"
           "hu.dwim.def.test.asd"
           "hu.dwim.def+swank.asd"
           "hu.dwim.def.asd"
           "hu.dwim.def+hu.dwim.delico.asd"))
    (hu.dwim.defclass-star (dwim-hu)
     :asd ("hu.dwim.defclass-star+hu.dwim.def.asd"
           "hu.dwim.defclass-star.documentation.asd"
           "hu.dwim.defclass-star.asd"
           "hu.dwim.defclass-star.test.asd"
           "hu.dwim.defclass-star+contextl.asd"
           "hu.dwim.defclass-star+swank.asd"
           "hu.dwim.defclass-star+hu.dwim.def+contextl.asd"))
    (hu.dwim.delico (dwim-hu)
     :asd ("hu.dwim.delico.asd"
           "hu.dwim.delico.documentation.asd"
           "hu.dwim.delico.test.asd"))
    (hu.dwim.dises (dwim-hu)
     :asd ("hu.dwim.dises.test.asd"
           "hu.dwim.dises.asd"
           "hu.dwim.dises.documentation.asd"))
    (hu.dwim.excosy (dwim-hu)
     :asd ("hu.dwim.excosy.test.asd"
           "hu.dwim.excosy.asd"
           "hu.dwim.excosy.documentation.asd"))
    (hu.dwim.genetic-programming (dwim-hu)
     :asd ("hu.dwim.genetic-programming.test.asd"
           "hu.dwim.genetic-programming.asd"
           "hu.dwim.genetic-programming.documentation.asd"))
    (hu.dwim.graphviz darcs "http://dwim.hu/darcs/hu.dwim.graphviz"
     :asd ("hu.dwim.graphviz.asd"
           "hu.dwim.graphviz.test.asd"
           "hu.dwim.graphviz.documentation.asd"))
    (hu.dwim.home (dwim-hu)
     :asd ("hu.dwim.home.documentation.asd"
           "hu.dwim.home.asd"
           "hu.dwim.home.all.asd"
           "hu.dwim.home.test.asd"))
    (hu.dwim.lazy-eval (dwim-hu)
     :asd ("hu.dwim.lazy-eval.test.asd"
           "hu.dwim.lazy-eval.asd"
           "hu.dwim.lazy-eval.documentation.asd"))
    (hu.dwim.logger (dwim-hu)
     :asd ("hu.dwim.logger.documentation.asd"
           "hu.dwim.logger.asd"
           "hu.dwim.logger.test.asd"))
    (hu.dwim.meta-model (dwim-hu)
     :asd ("hu.dwim.meta-model.test.asd"
           "hu.dwim.meta-model.documentation.asd"
           "hu.dwim.meta-model.asd"))
    (hu.dwim.model (dwim-hu)
     :asd ("hu.dwim.model.address.asd"
           "hu.dwim.model.documentation.asd"
           "hu.dwim.model.test.asd"
           "hu.dwim.model.asd"))
    (hu.dwim.new-project (dwim-hu)
     :asd ("hu.dwim.new-project.documentation.asd"
           "hu.dwim.new-project.test.asd"
           "hu.dwim.new-project.asd"))
    (hu.dwim.partial-eval (dwim-hu)
     :asd ("hu.dwim.partial-eval.test.asd"
           "hu.dwim.partial-eval.asd"
           "hu.dwim.partial-eval.documentation.asd"))
    (hu.dwim.perec darcs "http://dwim.hu/darcs/hu.dwim.perec"
     :asd ("hu.dwim.perec.test.asd"
           "hu.dwim.perec+iolib.asd"
           "hu.dwim.perec+hu.dwim.quasi-quote.xml.asd"
           "hu.dwim.perec+swank.asd"
           "hu.dwim.perec.documentation.asd"
           "hu.dwim.perec.sqlite.test.asd"
           "hu.dwim.perec.oracle.asd"
           "hu.dwim.perec.sqlite.asd"
           "hu.dwim.perec.postgresql.test.asd"
           "hu.dwim.perec.postgresql.asd"
           "hu.dwim.perec.asd"
           "hu.dwim.perec.oracle.test.asd"))
    (hu.dwim.quasi-quote darcs "http://dwim.hu/darcs/hu.dwim.quasi-quote"
     :asd ("hu.dwim.quasi-quote.asd"
           "hu.dwim.quasi-quote.xml.asd"
           "hu.dwim.quasi-quote.js.asd"
           "hu.dwim.quasi-quote.documentation.asd"
           "hu.dwim.quasi-quote.pdf.asd"
           "hu.dwim.quasi-quote.test.asd"
           "hu.dwim.quasi-quote.xml+cxml.asd"
           "hu.dwim.quasi-quote.css.asd"
           "hu.dwim.quasi-quote.xml+hu.dwim.quasi-quote.js.asd"))
    (hu.dwim.rdbms darcs "http://dwim.hu/darcs/hu.dwim.rdbms"
     :asd ("hu.dwim.rdbms.sqlite.test.asd"
           "hu.dwim.rdbms.sqlite.asd"
           "hu.dwim.rdbms.asd"
           "hu.dwim.rdbms.documentation.asd"
           "hu.dwim.rdbms.oracle.test.asd"
           "hu.dwim.rdbms.postgresql.asd"
           "hu.dwim.rdbms.test.asd"
           "hu.dwim.rdbms.postgresql.test.asd"
           "hu.dwim.rdbms.oracle.asd"))
    (hu.dwim.reader (dwim-hu)
     :asd ("hu.dwim.reader.test.asd"
           "hu.dwim.reader.documentation.asd"
           "hu.dwim.reader+hu.dwim.syntax-sugar.asd"
           "hu.dwim.reader.asd"
           "hu.dwim.reader+hu.dwim.walker.asd"))
    (hu.dwim.reiterate (dwim-hu)
     :asd ("hu.dwim.reiterate.asd"
           "hu.dwim.reiterate.documentation.asd"
           "hu.dwim.reiterate.test.asd"))
    (hu.dwim.remote-eval (dwim-hu)
     :asd ("hu.dwim.remote-eval.asd"
           "hu.dwim.remote-eval.documentation.asd"
           "hu.dwim.remote-eval.test.asd"))
    (hu.dwim.serializer darcs "http://dwim.hu/darcs/hu.dwim.serializer"
     :asd ("hu.dwim.serializer.test.asd"
           "hu.dwim.serializer.asd"
           "hu.dwim.serializer.documentation.asd"))

    ;; XXXXXXXXXXXXXXXXXXXX
    ;; Moved into git repository along with
    ;; git://dwim.hu/git/cl-graph
    ;; git://dwim.hu/git/commonqt
    ;; git://dwim.hu/git/iolib
    ;; git://dwim.hu/git/sbcl
    ;; git://dwim.hu/git/slime
    ;; git://dwim.hu/git/xcvb
    ;; use BRANCH "hu.dwim"
    ;; I use swank.asd from the slime project.

    ;; Moved to slime darcs or slime git repoository on dwim.hu ??
    ;; (hu.dwim.slime (dwim-hu)
    ;;  :asd none)

    (hu.dwim.stefil (dwim-hu)
     :asd ("hu.dwim.stefil.test.asd"
           "hu.dwim.stefil+hu.dwim.def.asd"
           "hu.dwim.stefil+swank.asd"
           "hu.dwim.stefil.asd"
           "hu.dwim.stefil.documentation.asd"
           "hu.dwim.stefil+hu.dwim.def+swank.asd"))
    (hu.dwim.syntax-sugar darcs "http://dwim.hu/darcs/hu.dwim.syntax-sugar"
     :asd ("hu.dwim.syntax-sugar.asd"
           "hu.dwim.syntax-sugar.unicode.asd"
           "hu.dwim.syntax-sugar+hu.dwim.walker.asd"
           "hu.dwim.syntax-sugar.documentation.asd"
           "hu.dwim.syntax-sugar.test.asd"))
    (hu.dwim.walker darcs "http://dwim.hu/darcs/hu.dwim.walker"
     :asd ("hu.dwim.walker.documentation.asd"
           "hu.dwim.walker.test.asd"
           "hu.dwim.walker.asd"))
    (hunchentoot (github "edicl"))
    (hunchentoot-auth (harmon)
     :asd ("hunchentoot-auth.asd"
           "hunchentoot-auth-test.asd"))
    (hunchentoot-blank (github "quek"))
    ;; Now also available as: https://github.com/slyrus/hunchentoot-cgi
    ;; perhaps with patches for the latest hunchentoot.
    (hunchentoot-cgi (harmon))
    (hunchentoot-vhost (harmon))
    (hyperdoc darcs "http://common-lisp.net/project/editor-hints/darcs/hyperdoc")
    (hyperobject (b9)
     :asd ("hyperobject.asd"
           "hyperobject-tests.asd"))
    (ia-x86 cvs pserver anonymous t common-lisp.net "/project/movitz/cvsroot")
    (iconv (github "quek" "cl-iconv"))
    (idna (github "antifuchs"))
    (ieee-floats (clnet darcs))
    (image-ops (github "mon-key"))
    (imago (clnet cvs)
     :asd ("src/imago.asd"))
    (imap (github "franzinc")
     :asd none)
    (incf-cl (github "jmbr"))
    (index-mapped-arrays (github "smithzvk")
     :asd ("index-mapped-arrays.asd"
           "index-mapped-arrays-test.asd"))
    (inet git "http://www.jarw.org.uk/lisp/cl-inet.git"
     :asd ("http/inet.http.asd"
           "inet-tests.asd"
           "inet.asd"))
    (info.read-eval-print.cl-mayu (github "quek"))
    (info.read-eval-print.climacs.dired (github "quek"))
    (info.read-eval-print.climacs.ext (github "quek"))
    (info.read-eval-print.editor (github "quek"))
    (info.read-eval-print.mecab (github "quek"))
    (info.read-eval-print.nando (github "quek"))
    (info.read-eval-print.reader (github "quek"))
    (info.read-eval-print.repl-tw (github "quek" "twitter-client"))
    (info.read-eval-print.web (github "quek"))
    (inotify (github "stassats"))
    ;; iolib used to be pulled as follows: (iolib (repo-or-cz) ...)
    ;; Switched to gitorious because static-vectors is there too.
    (iolib (gitorious)
     :asd ("examples/iolib.examples.asd"
           "src/iolib.asd"
           "src/iolib.base.asd"
           "src/iolib.multiplex.asd"
           "src/iolib.trivial-sockets.asd"
           "src/iolib-grovel.asd"
           "src/iolib.streams.asd"
           "src/iolib.pathnames.asd"
           "src/iolib.asdf.asd"
           "src/iolib.os.asd"
           "src/iolib.syscalls.asd"
           "src/iolib.sockets.asd"
           "src/iolib.conf.asd"
           "tests/iolib-tests.asd"))
    (iolib-simple-mux (github "vsedach")
     :asd none)
    (irc-logger (b9))
    (ironclad (github "froydnj"))
    (irs (github "kmi")
     :asd ("src/irs.asd"
           "apps/soa4all/soa4all.asd"
           "apps/cs-invocation/cs-invocation.asd"
           "apps/lhdl/lhdl.asd"
           "apps/sepc-to-bpmo/sepc-to-bpmo.asd"
           "apps/travel/travel.asd"
           "apps/travel/travel-publisher.asd"
           "apps/travel/travel-services.asd"
           "apps/nih/nih.asd"
           "apps/bpmo-to-irs/bpmo-to-irs.asd"
           "apps/math/math.asd"
           "apps/tutorial-iswc-2007/tutorial-iswc-2007.asd"
           "apps/tutorial-iswc-2007/tutorial-iswc-2007-services.asd"
           "apps/monitoring-engine/monitoring-engine.asd"
           "apps/trusted-travel/trusted-travel.asd"
           "apps/demo/demo.asd"
           "publisher/irs-publisher.asd"
           "tests/irs-tests.asd"
           "tests/irs-tests-core.asd"))
    (iso-media (github "slyrus"))
    (iso-3166-1 (github "stassats"))
    (iterate (clnet darcs))
    (j cvs pserver anonymous nil armedbear-j.cvs.sourceforge.net "/cvsroot/armedbear-j"
     :asd none)
    (jacolib (google-code svn)
     :asd ("jacolib-lisp/jacolib/jacolib.asd"))
    (jarw git "http://www.jarw.org.uk/lisp/cl-jarw.git"
     :asd ("jarw.asd"
           "queues.asd"))
    (jofrli (github "antifuchs"))
    (jpegmeta (google-code svn)
     :asd ("jpegmeta.asd"
           ;; "binary-data/com.gigamonkeys.binary-data.asd"
           ;; "macro-utilities/com.gigamonkeys.macro-utilities.asd"
           ))
    ;; XXXX: Is this an early version of parse-js ?
    (js-parser (github "gonzojive")
     :asd ("js-parser-tests.asd"
           "js-parser.asd"))
    (js-toolkit (github "vsedach"))
    (jsown (github "madnificent")
     :asd ("jsown.asd"
           "tests/jsown-tests.asd"))
    (jwacs darcs "http://chumsley.org/jwacs/unstable"
     :asd ("jwacs.asd"
           "jwacs-tests.asd"))
    (kayou (github "patzy"))
    (kilns  (github "sellout" "Kilns"))
    (kmrcl (b9)
     :asd ("kmrcl.asd"
           "kmrcl-tests.asd"))
    (kpax darcs "http://www.beta9.be/darcs/kpax"
     :asd ("kpax-core.asd"
           "kpax-examples.asd"
           "kpax-mod-lisp.asd"
           "kpax-paserve.asd"
           "kpax-s-http-server.asd"
           "kpax.asd"))
    (kyoto-persistence (github "kraison"))
    (langutils (github "kraison")
     :asd ("langutils/langutils.asd"
           "meta/meta.asd"
           "stdutils/stdutils.asd"
           "stdutils/utils.asd"))
    (lassie (melis))
    (latex-table (github "tpapp"))
    (lens (github "willijar" "LENS"))
    (let-plus (github "tpapp"))
    (lets (github "g000001" "LetS")
     :asd ("lets.asd"
           "lets-tests.asd"))
    ;; XXXX: This user has disappeared on github.
    ;; (lexer (github "turbo24prg"))
    (liards (clnet darcs))
    (lice (repo-or-cz)
     :asd ("src/lice.asd"))
    (lift (github "gwkkwg")
     :asd ("lift.asd"
           "lift-test.asd"
           "lift-documentation.asd"))
    (limited-thread-taskmaster (github "billstclair"))
    ;; http://common-lisp.net/project/linedit
    (linedit (clnet git)
     :asd ("linedit.asd"
           "terminfo.asd"))
    (linebreaker (github "xach"))
    (linj (github "xach"))
    (lint (github "g000001"))
    ;; Update failed 2011-10-12
    ;; failed to create lock directory for `/cvsroot/lisa/lisa'
    ;; Update failed 2011-10-26 for same reason.
    ;; Update failed 2011-11-30 for same reason.
    (lisa (sourceforge cvs))
    (lisp-critic (github "g000001"))
    (lisp-matrix (github "blindglobe"))
    (lisp-on-lines (github "drewc")
     :asd ("lisp-on-lines.asd"
           "lisp-on-lines-ucw.asd"))
    (lisp-on-rails (github "quek")
     :asd ("lack-hunchentoot.asd"
           "example/blog/config/blog.asd"
           "railties.asd"
           "action-pack.asd"
           "active-record.asd"
           "lack.asd"
           "active-support.asd"))
    (lisp-unit (github "OdonataResearchLLC"))
    ;; XXXX: we need an asd rule that says "link every asd file in tree"
    (lispbuilder (google-code svn)
     :asd ("lispbuilder-cal3d/lispbuilder-cal3d-examples.asd"
           "lispbuilder-cal3d/lispbuilder-cal3d.asd"
           "lispbuilder-clawk/lispbuilder-clawk.asd"
           "lispbuilder-lexer/lispbuilder-lexer.asd"
           "lispbuilder-net/lispbuilder-net-cffi.asd"
           "lispbuilder-net/lispbuilder-net-examples.asd"
           "lispbuilder-net/lispbuilder-net.asd"
           "lispbuilder-opengl/lispbuilder-opengl-1-1.asd"
           "lispbuilder-opengl/lispbuilder-opengl-1-2.asd"
           "lispbuilder-opengl/lispbuilder-opengl-1-3.asd"
           "lispbuilder-opengl/lispbuilder-opengl-examples.asd"
           "lispbuilder-opengl/lispbuilder-opengl-ext.asd"
           "lispbuilder-openrm/lispbuilder-openrm-base.asd"
           "lispbuilder-openrm/lispbuilder-openrm-binaries.asd"
           "lispbuilder-openrm/lispbuilder-openrm-cffi.asd"
           "lispbuilder-openrm/lispbuilder-openrm-examples.asd"
           "lispbuilder-openrm/lispbuilder-openrm-native-examples.asd"
           "lispbuilder-openrm/lispbuilder-openrm-native.asd"
           "lispbuilder-openrm/lispbuilder-openrm-sdl-examples.asd"
           "lispbuilder-openrm/lispbuilder-openrm-sdl.asd"
           "lispbuilder-openrm/lispbuilder-openrm.asd"
           "lispbuilder-regex/lispbuilder-regex.asd"
           "lispbuilder-sdl-gfx/lispbuilder-sdl-gfx-binaries.asd"
           "lispbuilder-sdl-gfx/lispbuilder-sdl-gfx-cffi.asd"
           "lispbuilder-sdl-gfx/lispbuilder-sdl-gfx-examples.asd"
           "lispbuilder-sdl-gfx/lispbuilder-sdl-gfx.asd"
           "lispbuilder-sdl-image/lispbuilder-sdl-image-binaries.asd"
           "lispbuilder-sdl-image/lispbuilder-sdl-image-cffi.asd"
           "lispbuilder-sdl-image/lispbuilder-sdl-image-examples.asd"
           "lispbuilder-sdl-image/lispbuilder-sdl-image.asd"
           "lispbuilder-sdl-mixer/lispbuilder-sdl-mixer-binaries.asd"
           "lispbuilder-sdl-mixer/lispbuilder-sdl-mixer-cffi.asd"
           "lispbuilder-sdl-mixer/lispbuilder-sdl-mixer-examples.asd"
           "lispbuilder-sdl-mixer/lispbuilder-sdl-mixer.asd"
           "lispbuilder-sdl-ttf/lispbuilder-sdl-ttf-binaries.asd"
           "lispbuilder-sdl-ttf/lispbuilder-sdl-ttf-cffi.asd"
           "lispbuilder-sdl-ttf/lispbuilder-sdl-ttf-examples.asd"
           "lispbuilder-sdl-ttf/lispbuilder-sdl-ttf.asd"
           "lispbuilder-sdl/cocoahelper.asd"
           "lispbuilder-sdl/lispbuilder-sdl-assets.asd"
           "lispbuilder-sdl/lispbuilder-sdl-base.asd"
           "lispbuilder-sdl/lispbuilder-sdl-binaries.asd"
           "lispbuilder-sdl/lispbuilder-sdl-cffi.asd"
           "lispbuilder-sdl/lispbuilder-sdl-cl-vectors-examples.asd"
           "lispbuilder-sdl/lispbuilder-sdl-cl-vectors.asd"
           "lispbuilder-sdl/lispbuilder-sdl-examples.asd"
           "lispbuilder-sdl/lispbuilder-sdl-vecto-examples.asd"
           "lispbuilder-sdl/lispbuilder-sdl-vecto.asd"
           "lispbuilder-sdl/lispbuilder-sdl.asd"
           "lispbuilder-sdl/trivial-garbage.asd"
           "lispbuilder-windows/lispbuilder-windows-examples.asd"
           "lispbuilder-windows/lispbuilder-windows.asd"
           "lispbuilder-yacc/lispbuilder-yacc.asd"))
    (lispdev (bitbucket "skolos")
     :asd none)
    (lisplog (github "billstclair" "Lisplog"))
    (lispmud (github "Valera" "LispMud")
     :asd ("lispmud.asd"
           "mudsketcher.asd"))
    ;; XXXX: maybe change project name to lisppaste
    (lisppaste2 cvs pserver anonymous t common-lisp.net "/project/lisppaste/cvsroot"
     :asd ("lisppaste.asd"))
    (literate-lisp (github "smithzvk"))
    (lla (github "tpapp"))
    (llvm  (github "sellout" "CL-LLVM"))
    (lml (b9)
     :asd ("lml.asd"
           "lml-tests.asd"))
    (lml2 (b9)
     :asd ("lml2.asd"
           "lml2-tests.asd"))
    (local-time (clnet darcs)
     :asd ("cl-postgres+local-time.asd"
           "local-time.asd"
           "local-time.test.asd"))
    (log4cl (github "7max"))
    (log5 (github "gwkkwg")
     :asd ("log5.asd"
           "log5-test.asd"))
    (loom (github "sellout" "LOOM"))
    (lotzo (github "patzy")
     :asd ("notes/notes.asd"
           "motd/motd.asd"
           "lotzo.asd"
           "announces/announces.asd"
           "logging/log.asd"))
    (lredis (github "death"))
    (lsw2 (google-code svn)
     :asd none)
    (lw-compat darcs "http://common-lisp.net/project/closer/repos/lw-compat")
    (maclisp-compat (github "g000001" "MacLISP-compat")
     :asd ("maclisp-compat.asd"
           "misc/maclisp.asd"))
    (macrophp (github "valeryz" "MacroPHP"))
    (mailbox-plus (github "austinhaas")
     :asd ("mailbox-plus.asd"
           "mailbox-plus-tests.asd"))
    (maild (github "franzinc")
     :asd none)
    ;; Also available from git://github.com/ilitirit/manardb.git
    ;; Maybe that's the real master?  double check other msi projects
    (manardb git "http://cl-www.msi.co.jp/projects/manardb/manardb.git"
     :asd ("manardb.asd"
           "manardb-test.asd"))
    (markup git "http://www.jarw.org.uk/lisp/cl-markup.git")
    (math-extensions (github "sellout"))
    (matlisp cvs pserver anonymous nil matlisp.cvs.sourceforge.net "/cvsroot/matlisp"
     :asd none)
    (maxima (sourceforge cvs)
     :asd ("src/maxima.asd"))
    (mbe (github "g000001"))
    (mc git "git://git.informatimago.com/public/mc"
     :asd none)
    (mcclim (clnet cvs)
     :asd ("Apps/Functional-Geometry/functional-geometry.asd"
           "Apps/Scigraph/scigraph.asd"
           "Drei/cl-automaton/automaton.asd"
           "ESA/esa.asd"
           "Experimental/freetype/mcclim-freetype.asd"
           "Experimental/freetype/mcclim-truetype.asd"
           "Experimental/tree-with-cross-edges/mcclim-tree-with-cross-edges.asd"
           "Extensions/conditional-commands/conditional-commands.asd"
           "clim-examples.asd"
           "clim-listener.asd"
           "clouseau.asd"
           "mcclim-gif-bitmaps.asd"
           "mcclim-jpeg-bitmaps.asd"
           "mcclim-tiff-bitmaps.asd"
           "mcclim.asd"))
    (mcclim-uim (github "quek"))
    (mcpixel (github "ahefner" "McPixel"))
    (md5 (b9))
    (media git "http://www.jarw.org.uk/lisp/media.git")
    (mel-base darcs "http://www.crispylogics.com/opensource/repos/mel-base")
    ;; XXXX: mel-base-old got a bunch of updates on jan 18 2010 that were
    ;; old ... jan to nov of 2009
    (mel-base-old darcs "http://common-lisp.net/project/mel-base/darcs/mel-base"
     :asd none)
    (message-stream (github "austinhaas")
     :asd ("message-stream.asd"
           "message-stream-tests.asd"))
    (meta-sexp (github "vy"))
    (metabang-bind (github "gwkkwg")
     :asd ("metabang-bind.asd"
           "metabang-bind-test.asd"))
    (metacopy (clnet darcs)
     :asd ("metacopy.asd"
           "metacopy-test.asd"))
    (metatilities darcs "http://common-lisp.net/project/metatilities"
     :asd ("metatilities.asd"
           "metatilities-test.asd"))
    (metatilities-base darcs "http://common-lisp.net/project/metatilities-base"
     :asd ("metatilities-base.asd"
           "metatilities-base-test.asd"))
    (method-versions (github "nklein"))
    (mgl (melis)
     :asd ("mgl-test.asd"
           "mgl.asd"
           "mgl-example.asd"
           "mgl-visuals.asd"))
    (mixalot (github "ahefner")
     :asd ("mixalot.asd"
           "mixalot-mp3.asd"
           "mpg123-ffi.asd"))
    (micmac (melis)
     :asd ("micmac.asd"
           "micmac-test.asd"))
    (mod_lisp svn "http://www.fractalconcept.com:8000/public/open-source/mod_lisp"
     :asd none)
    (modf (github "smithzvk"))
    (modf-fset (github "smithzvk"))
    (modf-funds (github "smithzvk"))
    (modlisp (b9 "cl-modlisp"))
    (mon (github "mon-key" "mon-systems-cl"))
    (monkeylib-markup-html (github "gigamonkey"))
    (monkeylib-markup-xml (github "gigamonkey"))
    (montezuma (google-code svn)
     :asd ("montezuma.asd"
           "contrib/montezuma-indexfiles/montezuma-indexfiles.asd"
           "lucene-in-action/lucene-in-action-tests.asd"))
    ;; XXXXXX maybe rename project
    (mop-features darcs "http://common-lisp.net/project/closer/repos/mop-features"
     :asd ("mop-feature-tests.asd"))
    (mop-utils darcs "http://common-lisp.net/project/submarine/darcs/mop-utils")
    (moptilities darcs "http://common-lisp.net/project/moptilities"
     :asd ("moptilities.asd"
           "moptilities-test.asd"))
    (mounit (github "quek"))
    (movitz cvs pserver anonymous t common-lisp.net "/project/movitz/cvsroot")
    (mpd (github "stassats"))
    (mtlisp (github "mtravers"))
    (mutest (github "vseloved"))
    (mw-equiv svn "http://svn.foldr.org/~michaelw/mw-equiv/trunk")
    (mw-tiny-clos git "http://www.foldr.org/~michaelw/projects/mw-tiny-clos.git")
    (mycl-util darcs "http://common-lisp.net/project/bdb/darcs/mycl-util")
    ;; XXXX: This user is now missing on github.
    ;; (n3 (github "turbo24prg"))
    (named-readtables darcs "http://common-lisp.net/project/editor-hints/darcs/named-readtables")
    (net-xml-generator (github "franzinc")
     :asd none)
    ;; Update failed on 2011-10-12
    ;; Update failed on 2011-10-26
    (netkit darcs "http://fresh.homeunix.net/~luke/misc/repo/netkit"
     :asd none)
    (nfs (github "franzinc")
     :asd none)
    (nibbles (github "froydnj"))
    (nil-compat (github "g000001"))
    (nio (clnet cvs)
     :asd ("nio.asd"
           "event-notification.asd"))
    (nlisp (sourceforge svn))
    (ntservice (github "franzinc")
     :asd none)
    (nuclblog (harmon)
     :asd ("nuclblog.asd"
           "nuclblog-demo.asd"))
    (nuts (github "vseloved")
     :asd ("nuts.asd"
           "nuts-clsql.asd"
           "nuts-core.asd"))
    ;; Repository problem on 2011-11-30.
    (objcffi (repo-or-cz))
    (ocml (github "kmi"))
    (ogg (github "stassats"))
    (okra (github "aerique")
     :asd ("okra-bindings.asd"
           "okra-bindings-generator.asd"
           "okra-common.asd"
           "okra-mygui.asd"
           "okra.asd"))
    (ometa hg "http://subvert-the-dominant-paradigm.net/repos/hgwebdir.cgi/ometa")
    (open-axiom (sourceforge svn)
     :asd none)
    (opticl (github "slyrus")
     :asd ("opticl-doc.asd"
           "opticl.asd"))
    (opticl-test (github "slyrus"))
    (opticl-examples (github "slyrus"))
    (option-9 git "http://pages.cs.wisc.edu/~psilord/lisp-public/public-repos-lisp/option-9.git"
     :asd none)
    (osicat (clnet git)
     :asd ("osicat.asd"
           "osicat-tests.asd"))
    (outbreak (github "patzy"))
    (package-local-nicknames (github "3b"))
    (page-ranges (github "xach"))
    (paktahn (github "skypher")
     :asd none)
    (pango (github "BradWBeer" "CL-Pango"))
    ;; XXXXXXXXXXX my dir is empty ... find the code, change the asd setting
    (paragent (google-code svn)
     :asd none)
    (paren-events (github "gonzojive"))
    (paren-files (github "gonzojive"))
    (paren-psos (github "gonzojive"))
    (paren-test (github "gonzojive")
     :asd none)
    (paren-util (github "gonzojive"))
    ;; XXXXXXXXXX: There is a parenscript project on github.  Is it better?
    ;; (parenscript (github "3b") :asd none)
    (parenscript git "http://common-lisp.net/project/parenscript/git/parenscript")
    (parse-declarations (clnet darcs)
     :asd ("parse-declarations-1.0.asd"))
    (parse-html darcs "http://common-lisp.net/project/bese/repos/parse-html")
    (parse-js (github "marijnh"))
    (parse-number (github "sharplispers"))
    (pastebin.com-api (github "Lovesan"))
    (pastel.blocky (github "dto")
     :asd none)
    ;; (patg (clnet ))  XXXXXXXXXXX Subversion ???
    (patron (github "vy"))
    ;; XXXX: blog entry says persistent-sheeple is being renamed.  code has
    ;; disappeared from github.
    ;; (persistent-sheeple (github "sykopomp"))
    (pcall (github "marijnh")
     :asd ("pcall.asd"
           "pcall-queue.asd"))
    (pcl-practicals (github "gigamonkey"))
    (perfpiece (github "luismbo"))
    (pg (clnet cvs))
    (phoros (github "trebb")
     :asd ("phoros.asd"
           "phoros-test.asd"))
    (phoml (github "trebb"))
    (pileup (github "nikodemus"))
    (pipes (b9))
    (pipes-edsl (github "pkhuong" "Pipes")
     :asd none)
    (pithy-xml (github "frodef"))
    ;; http://informatimago.com/develop/lisp/
    (pjb-lisp git "git://git.informatimago.com/public/lisp"
     :asd ("clisp/com.informatimago.clisp.asd"
           "susv3/com.informatimago.susv3.asd"
           "tools/com.informatimago.common-lisp.tools.make-depends.asd"
           "clext/com.informatimago.clext.asd"
           "cl-posix/cliki/cliki.asd"
           "cl-posix/cliki/clposixcliki.asd"
           "clmisc/com.informatimago.clmisc.asd"
           "common-lisp/diagram/com.informatimago.common-lisp.diagram.asd"
           "common-lisp/html-parser/com.informatimago.common-lisp.html-parser.asd"
           "common-lisp/csv/com.informatimago.common-lisp.csv.asd"
           "common-lisp/http/com.informatimago.common-lisp.http.asd"
           "common-lisp/cesarum/com.informatimago.common-lisp.cesarum.asd"
           "common-lisp/regexp/com.informatimago.common-lisp.regexp.asd"
           "common-lisp/rfc2822/com.informatimago.common-lisp.rfc2822.asd"
           "common-lisp/cxx/com.informatimago.common-lisp.cxx.asd"
           "common-lisp/rfc3548/com.informatimago.common-lisp.rfc3548.asd"
           "common-lisp/invoice/com.informatimago.common-lisp.invoice.asd"
           "common-lisp/picture/com.informatimago.common-lisp.picture.asd"
           "common-lisp/graphviz/com.informatimago.common-lisp.graphviz.asd"
           "common-lisp/lisp-reader/com.informatimago.common-lisp.lisp-reader.asd"
           "common-lisp/bank/com.informatimago.common-lisp.bank.asd"
           "common-lisp/lisp-text/com.informatimago.common-lisp.lisp-text.asd"
           "common-lisp/lisp/com.informatimago.common-lisp.lisp.asd"
           "common-lisp/interactive/com.informatimago.common-lisp.interactive.asd"
           "common-lisp/html-generator/com.informatimago.common-lisp.html-generator.asd"
           "common-lisp/lisp-sexp/com.informatimago.common-lisp.lisp-sexp.asd"
           "common-lisp/heap/com.informatimago.common-lisp.heap.asd"
           "common-lisp/arithmetic/com.informatimago.common-lisp.arithmetic.asd"
           "common-lisp/com.informatimago.common-lisp.asd"
           "common-lisp/unix/com.informatimago.common-lisp.unix.asd"
           "common-lisp/parser/com.informatimago.common-lisp.parser.asd"
           "common-lisp/ed/com.informatimago.common-lisp.ed.asd"
           "common-lisp/data-encoding/com.informatimago.common-lisp.data-encoding.asd"
           "common-lisp/html-base/com.informatimago.common-lisp.html-base.asd"
           "sbcl/com.informatimago.sbcl.asd"))
    (pjb-small-cl-pgms git "git://git.informatimago.com/public/small-cl-pgms"
     :asd none)
    (planet-wars (melis)
     :asd ("planet-wars.asd"
           "planet-wars-util.asd"
           "proxy-bot.asd"))
    (planks (github "drewc"))
    ;; XXXX: This repository has disappeared on github.
    ;; (plantae (github "patzy"))
    ;; XXXX: rename to xpath
    (plexippus-xpath (clnet darcs)
     :asd ("xpath.asd"))
    (plop (google-code svn)
     :asd ("src/plop.asd"))
    (png-read (github "Ramarren"))
    (poiu (clnet git "qitab"))
    (ponzu.db (github "fukamachi")
     :asd ("ponzu.db.asd"
           "ponzu.db-test.asd"))
    (portableaserve (sourceforge cvs)
     :asd ("acl-compat/acl-compat.asd"
           "aserve/aserve.asd"
           "aserve/htmlgen/htmlgen.asd"
           "aserve/webactions/webactions.asd"
           ;; "libs/cl-ppcre/cl-ppcre-test.asd"
           ;; "libs/cl-ppcre/cl-ppcre.asd"
           ;; "libs/puri-1.3.1/puri.asd"
           ))
    (postmodern (github "marijnh" "Postmodern")
     :asd ("postmodern.asd"
           "cl-postgres.asd"))
    (postoffice (b9))
    (prepl (gitorious))
    (pretty-function (clnet darcs))
    (protobuf (github "brown")
     :asd ("example/protobuf-example.asd"
           "protobuf-test.asd"
           "protobuf.asd"
           "varint/varint-test.asd"
           "varint/varint.asd"))
    (pgsql (github "franzinc")
     :asd none)
    (ptester (b9))
    (pubmed (b9))
    (puri (b9))
    (push (github "patzy"))
    ;; XXXX: I had to permanently accepted a bogus expired certificate.
    (pvs svn "https://spartan.csl.sri.com/svn/public/pvs/trunk"
     :asd none)
    (qbook darcs "http://common-lisp.net/project/bese/repos/qbook")
    (qpj1 (github "g000001"))
    (quek (github "quek"))
    (quicklisp-bootstrap (github "quicklisp")
     :asd none)
    (quicklisp-client (github "quicklisp")
     :asd ("quicklisp.asd"))
    (quicklisp-projects (github "quicklisp")
     :asd none)
    (quicklisp-slime-helper (github "quicklisp"))
    (quickproject (github "xach"))
    ;; XXXX: rename to quicktime if possible
    ;; XXXX: try loading it and resolve all dependencies
    (quicktime-ffi (harmon)
     :asd ("quicktime.asd"
           "quicktime-init.asd"
           "quicktime-test.asd"))
    (quux-time (clnet git "qitab"))
    (raylisp (github "nikodemus")
     :asd ("raylisp.asd"
           "raylisp-gui.asd"))
    ;; XXXX: This user is now missing on github.
    ;; (rdf-store (github "turbo24prg"))
    ;; XXXX: This user is now missing on github.
    ;; (rdf-utils (github "turbo24prg"))
    (readline (b9 "cl-readline"))
    (recognize (github "3b")
     :asd ("recognize.asd"
           "recognize-demo.asd"))
    ;; Emacs extensions for lisp.
    (redshank darcs "http://www.foldr.org/~michaelw/projects/redshank"
     :asd none)
    (regenerate-websites (github "gwkkwg"))
    (repo-install (github "jpalmucci"))
    (restas (github "archimag"))
    (restas-planet (github "archimag"))
    (retrospectiff (github "slyrus")
     :asd ("retrospectiff-test.asd"
           "retrospectiff.asd"))
    (reversi (b9))
    (rfc2109 (clnet darcs))
    (rfc2388 (clnet cvs))
    (rfc2822 (clnet cvs))
    (rjson (github "gonzojive"))
    (rlc (b9))
    (roll-cl (google-code svn)
     :asd ("/src/lisp/socklib/roll-socklib.asd"
           "/src/lisp/util/roll-util.asd"
           "/src/lisp/gnuplot/roll-gnuplot.asd"
           "/src/lisp/roll.asd"
           "/src/lisp/math/math.asd"
           "/src/lisp/math/roll-math.asd"
           "/src/lisp/geo/roll-geo.asd"
           "/src/lisp/search/roll-search.asd"
           "/src/lisp/gui/roll-gui.asd"))
    (ropes (github "Ramarren"))
    (roto-mortar (github "nklein" "Roto-Mortar"))
    (routes (github "archimag" "cl-routes"))
    (rss (b9 "cl-rss"))
    (rt (b9))
    (rtf darcs "http://common-lisp.net/project/bese/repos/rtf")
    (rtorrent-controller (github "stassats"))
    (rucksack (clnet cvs)
     :asd ("rucksack.asd"
           "tests/rucksack-test.asd"))
    (rulisp (github "archimag"))
    (rutils (github "vseloved"))
    (s-base64 darcs "http://www.beta9.be/darcs/s-base64")
    (s-http-client darcs "http://www.beta9.be/darcs/s-http-client")
    (s-http-server darcs "http://www.beta9.be/darcs/s-http-server")
    (s-sysdeps darcs "http://www.beta9.be/darcs/s-sysdeps")
    (s-utils darcs "http://www.beta9.be/darcs/s-utils")
    (s-xml (clnet cvs))
    (s-xml-rpc (clnet cvs))
    ;; Update failed 2011-10-12
    ;; Repository problem on 2011-11-30.
    ;; Your configuration specifies to merge with the ref 'master' from the remote, but no
    ;; such ref was fetched.
    (salza2 (xach))
    (sapaclisp (clnet cvs))
    (sb-bench (github "nikodemus"))
    (sb-cga (github "nikodemus"))
    (sb-cpu-affinity (github "nikodemus"))
    (sb-daemon (github "nikodemus"))
    (sb-lset (github "nikodemus")
     :asd none)
    ;; XXXX: this repository is missing now
    ;; (sb-mailbox (github "nikodemus")
    ;;  :asd none)
    (sb-vector-io (github "nikodemus"))
    (sbcl (sourceforge git)
     :asd none)
    (4store (github "mon-key" "sbcl-4store"))
    ;;(sbcl-git git "git://git.boinkor.net/sbcl"
    ;; :asd none)
    (score (github "emoon" "Score")
     :asd ("src/Score.asd"))
    ;; XXXX:  Is this one better?  stored in screamer-or-cz
    ;;(screamer (repo-or-cz)
    ;;:asd none)
    (screamer (github "nikodemus"))
    (series (sourceforge cvs))
    (series-ext (github "g000001"))
    (sheeple (github "sykopomp"))
    (sheldonbot (github "vsedach" "SheldonBot"))
    (shibuya.lisp (github "g000001"))
    (shuffletron (github "ahefner"))
    ;; XXXX: This repository appears to be the canonical one, but is
    ;; sicl-OLD from "http://dept-info.labri.fr/~strandh/SICL/SICL.git"
    ;; still getting updates?
    (sicl (clnet git "sicl" "SICL")
     :asd ("Lisp-Unit/lisp-unit.asd"
           "Code/Cons-high/cons-high.asd"
           "Code/Loop/loop.asd"
           "Code/Reader/read.asd"
           "Code/sicl.asd"
           "Code/Format/format.asd"
           "Code/Sequences/sequences.asd"))
    (simple-date-time (github "quek"))
    ;; Another sicl project -- a SPARQL implementation.
    ;; (sicl (github "turbo24prg") :asd none)
    (simple-rgb (google-code svn))
    (single-threaded-ccl (clnet git "qitab")
     :asd none)
    ;; Update failed 2011-10-12
    ;; Repository problem on 2011-11-30.
    ;; Your configuration specifies to merge with the ref 'master' from the remote, but no
    ;; such ref was fetched.
    (skippy (xach))
    (slime (clnet cvs)
     :asd ("swank.asd"))
    (slime-cover (github "stassats")
     :asd none)
    (slime-proxy (github "3b")
     :asd ("slime-proxy.asd"
           "contrib/slime-parenscript/swank-parenscript.asd"))
    ;; Update failed 2011-10-12
    (slitch (github "sharplispers")
     :asd ("src/netlib.asd"))

    ;; (slitch darcs "http://fresh.homeunix.net/~luke/misc/repo/slitch"
    ;;  :asd ("src/netlib.asd"))
    (slurp (github "brown"))
    (smarkup (github "slyrus")
     :asd ("smarkup.asd"
           "smarkup-test.asd"))
    (snow (clnet svn)
     :asd ("src/lisp/snow/snow.asd"
           ;; "lib/named-readtables/named-readtables.asd"
           ;; "lib/cl-utilities-1.2.4/cl-utilities.asd"
           ;; "lib/cells/utils-kt/utils-kt.asd"
           ;; "lib/cells/gui-geometry/gui-geometry.asd"
           ;; "lib/cells/cells.asd"
           ;; "lib/cells/cells-test/cells-test.asd"
           ))
    (sorting (github "gugamilare")
     :asd ("sorting.asd"
           "sorting-benchmark.asd"))
    (source-compare (github "g000001"))
    ;; XXXX: 2011-02-03 error: not a repository
    ;; XXXX: 2011-03-24 same thing
    ;; XXXX: 2011-05-31 cannot connect to server
    ;; Update failed 2011-10-12
    ;; (spatial-trees darcs "http://rvw.doc.gold.ac.uk/sullivan/darcs/spatial-trees")
    (spectacle (github "slyrus"))
    (split-sequence (github "sharplispers"))
    (squirl (github "sykopomp"))
    (srfi-0 (github "g000001"))
    (srfi-1 (github "g000001"))
    (srfi-2 (github "g000001"))
    (srfi-3 (github "g000001"))
    (srfi-4 (github "g000001"))
    (srfi-5 (github "g000001"))
    (srfi-6 (github "g000001"))
    (srfi-7 (github "g000001"))
    (srfi-8 (github "g000001"))
    (srfi-10 (github "g000001"))
    (srfi-11 (github "g000001"))
    (srfi-16 (github "g000001"))
    (srfi-23 (github "g000001"))
    (srfi-26 (github "g000001"))
    (srfi-28 (github "g000001"))
    (srfi-42 (github "g000001"))
    (srfi-48 (github "g000001"))
    (srfi-61 (github "g000001"))
    (srfi-62 (github "g000001"))
    (srfi-78 (github "g000001"))
    (srfi-86 (github "g000001"))
    (srfi-87 (github "g000001"))
    (srfi-98 (github "g000001"))
    ;; XXXX: May be obsolete.
    (srfi-compat (github "g000001"))
    (sse-expr (github "deplinenoise")
     :asd none)
    (sst (github "vy"))
    (st-json (github "marijnh" "ST-JSON"))
    (starlisp (github "g000001" "Starlisp-simulator"))
    (static-vectors (gitorious "iolib")
     :asd ("src/static-vectors.asd"))
    (storable-functions (github "gugamilare")
     :asd ("cl-store+functions.asd"
           "storable-functions.asd"
           "cl-store+functions-tests.asd"
           "storable-functions-tests.asd"))
    (storage (github "stassats"))
    (stumpwm git "git://git.savannah.nongnu.org/stumpwm.git")
    (style-checker-1 (github "g000001"))
    (submarine (clnet darcs)
     :asd ("submarine.asd"
           "submarine-tests.asd"))
    (subtitles (github "stassats"))
    (sw-db (github "lnostdal" "SW-DB"))
    (sw-http (github "lnostdal" "SW-HTTP"))
    (sw-mvc (github "lnostdal" "SW-MVC"))
    (sw-stm (github "lnostdal" "SW-STM"))
    (swank-client (github "brown")
     :asd ("swank-client.asd"
           "swank-client-test.asd"))
    (swank-client-usocket (github "pf")
     :asd none)
    ;; Efficient byte swapping for SBCL.
    (swap-bytes (github "stassats"))
    (sykobot (github "sykopomp"))
    ;;XXXXXXXXXXXXXXXXXXXX  all files were deleted
    ;;XXXXXXXXXXXXXXXXXXXX investigate
    ;; (sykosomatic (github "sykopomp")
    ;;  :asd ("sykosomatic.util.asd"
    ;;        "sykosomatic.core.asd"
    ;;        "sykosomatic.network.asd"
    ;;        "sykosomatic.object.asd"
    ;;        "sykosomatic.parser.asd"
    ;;        "sykosomatic.test.asd"))
    ;; Not present on 2011-11-30 ... move code someplace.
    ;; (symbol-munger (github "bobbysmith007"))
    ;; Repository was renamed at some point.  SymbolicWeb is now a Clojure project.
    (symbolicweb (github "lnostdal" "old-SymbolicWeb")
     :asd ("symbolicweb.asd"
           "symbolicweb-examples.asd"
           "symbolicweb-jquery.asd"))
    (t-system (gitorious)
     :asd none)
    ;; Not present on 2011-11-30 ... move code someplace.
    ;; (talcl (github "bobbysmith007"))
    (tao-compat (github "g000001"))
    (tapulli (google-code svn)
     :asd ("tag-pool/tag-pool.asd"))
    (teepeedee2 (github "vii")
     :asd ("teepeedee2.asd"
           "teepeedee2-test.asd"
           ;; "addons/alexandria/alexandria-tests.asd"
           ;; "addons/alexandria/alexandria.asd"
           ;; "addons/cl-cont/cl-cont-test.asd"
           ;; "addons/cl-cont/cl-cont.asd"
           ;; "addons/parenscript/parenscript.asd"
           ;; "addons/trivial-garbage/trivial-garbage.asd"
           ))
    (telent-clx (github "franzinc")
     :asd none)
    (terse-ppcre (google-code svn))
    (tester (github "franzinc"))
    (texinfo-docstrings (gitorious "iolib"))
    (the (google-code svn "cl-the"))
    (thread-pool (github "kiuma"))
    (tibly (google-code svn)
     :asd none)
    (thinlisp (github "vsedach" "Thinlisp-1.1")
     :asd none)
    (tiff-ffi (harmon)
     :asd ("tiff-ffi.asd"
           "tiff-ffi-gen.asd"))
    (tiff4cl (github "mon-key" "tiff4cl-fork"))
    (tilde (github "xach"))
    (time-interval (harmon))
    (tinaa darcs "http://common-lisp.net/project/tinaa"
     :asd ("tinaa.asd"
           "tinaa-test.asd"))
    (titler (github "ryepup"))
    (tottori-nando (github "quek"))
    (towers (github "death"))
    (tpapp-utils (github "tpapp"))
    (tracking (github "stassats"))
    (trivial-backtrace git "http://common-lisp.net/project/trivial-backtrace/trivial-backtrace.git"
     :asd ("trivial-backtrace.asd"
           "trivial-backtrace-test.asd"))
    (trivial-bit-streams (github "Lovesan"))
    (trivial-configuration-parser svn "svn://unmutual.info/trivial-configuration-parser")
    (trivial-features darcs "http://common-lisp.net/~loliveira/darcs/trivial-features"
     :asd ("trivial-features.asd"
           "trivial-features-tests.asd"))
    (trivial-garbage darcs "http://common-lisp.net/~loliveira/darcs/trivial-garbage")
    (trivial-gray-streams (gitorious))
    (trivial-http darcs "http://common-lisp.net/project/trivial-http"
     :asd ("trivial-http.asd"
           "trivial-http-test.asd"))
    (trivial-https cvs pserver anonymous t common-lisp.net "/project/cl-plus-ssl/cvsroot")
    (trivial-http-old svn "svn://unmutual.info/trivial-http"
     :asd none)
    (trivial-lisp-webapp (github "smanek")
     :asd ("src/webapp.asd"))
    (trivial-shell (github "gwkkwg")
     :asd ("trivial-shell.asd"
           "trivial-shell-test.asd"))
    (trivial-timeout darcs "http://common-lisp.net/project/trivial-timeout"
     :asd ("trivial-timeout.asd"
           "trivial-timeout-test.asd"))
    (trivial-utf-8 (clnet darcs))
    (trubanc-lisp (github "billstclair")
     :asd ("trubanc.asd"
           "trubanc-loader.asd"
           ;; "systems/puri-1.5.1/puri.asd"
           ;; "systems/cl-fad-0.6.2/cl-fad.asd"
           ;; "systems/cl-base64-3.3.2/cl-base64.asd"
           ;; "systems/cl-who-0.11.1/cl-who.asd"
           ;; "systems/bordeaux-threads/bordeaux-threads.asd"
           ;; "systems/chunga-1.0.0/chunga.asd"
           ;; "systems/ironclad_0.26/ironclad.asd"
           ;; "systems/babel_0.3.1/babel-streams.asd"
           ;; "systems/babel_0.3.1/babel-tests.asd"
           ;; "systems/babel_0.3.1/babel.asd"
           ;; "systems/cl-ppcre-2.0.1/cl-ppcre-unicode.asd"
           ;; "systems/cl-ppcre-2.0.1/cl-ppcre.asd"
           ;; "systems/cl+ssl-2008-11-04/cl+ssl.asd"
           ;; "systems/flexi-streams-1.0.7/flexi-streams.asd"
           ;; "systems/md5-1.8.5/md5.asd"
           ;; "systems/slime/swank.asd"
           ;; "systems/alexandria/alexandria.asd"
           ;; "systems/alexandria/alexandria-tests.asd"
           ;; "systems/hunchentoot-1.0.0/hunchentoot.asd"
           ;; "systems/hunchentoot-1.0.0/hunchentoot-test.asd"
           ;; "systems/usocket-0.4.1/usocket.asd"
           ;; "systems/usocket-0.4.1/test/usocket-test.asd"
           ;; "systems/trivial-features/trivial-features-tests.asd"
           ;; "systems/trivial-features/trivial-features.asd"
           ;; "systems/rfc2388/rfc2388.asd"
           ;; "systems/cffi_0.10.4/cffi-tests.asd"
           ;; "systems/cffi_0.10.4/cffi.asd"
           ;; "systems/cffi_0.10.4/cffi-grovel.asd"
           ;; "systems/cffi_0.10.4/uffi-compat/uffi.asd"
           ;; "systems/cffi_0.10.4/cffi-uffi-compat.asd"
           ;; "systems/cffi_0.10.4/cffi-examples.asd"
           ;; "systems/cl-smtp/cl-smtp.asd"
           ;; "systems/split-sequence/split-sequence.asd"
           ;; "systems/trivial-gray-streams-2008-11-02/trivial-gray-streams.asd"
           ;; "systems/cybertiggyr-time/cybertiggyr-time.asd"
           ;; "systems/drakma-1.0.0/drakma.asd"
           ))
    (truledger (github "billstclair")
     :asd ("truledger.asd"
           "systems/cybertiggyr-time/cybertiggyr-time.asd"))
    (ucw-presentations darcs "http://common-lisp.net/project/ucw/repos/ucw-presentations")
    (ucw_ajax darcs "http://common-lisp.net/project/ucw/repos/ucw_ajax"
     :asd none)
    (ucw_dev darcs "http://common-lisp.net/project/ucw/repos/ucw_dev"
     :asd ("ucw.asd"))
    (uffi (b9)
     :asd ("uffi.asd"
           "uffi-tests.asd"))
    (umlisp (b9)
     :asd ("umlisp.asd"
           "umlisp-tests.asd"))
    (umlisp-orf (b9)
     :asd ("umlisp-orf.asd"
           "umlisp-orf-tests.asd"))
    (uncl (github "fukamachi"))
    (unet (github "nklein"))
    (unicly (github "mon-key"))
    (until-it-dies (github "sykopomp")
     :asd ("until-it-dies.base.asd"
           "until-it-dies.examples.asd"
           "until-it-dies.asd"
           "until-it-dies.sound.asd"
           "until-it-dies.graphics.asd"))
    (upstream (github "nikodemus")
     :asd none)
    (uranus (github "g000001" "Uranus"))
    (uri (github "franzinc")
     :asd none)
    (uri-template (github "vsedach")
     :asd ("uri-template.asd"
           "uri-template.test.asd"))
    ;; Not present in bknr/third_party.
    (url-rewrite darcs "http://common-lisp.net/~loliveira/ediware/url-rewrite")
    (usenet-legend (github "xach"))
    ;; I used to pull from http://git.nklein.com/lisp/libs/userial.git
    ;; which the web site claims is the main repository.
    (userial (github "nklein")
     :asd ("userial.asd"
           "userial-tests.asd"))
    (usocket svn "svn://common-lisp.net/project/usocket/svn/usocket/trunk"
     :asd ("usocket.asd"
           "usocket-test.asd"))
    (vacietis (github "vsedach" "Vacietis")
     :asd ("vacietis.asd"
           "vacietis.test.asd"))
    (vana (github "sgrove")
     :asd none)
    (vana-inflector (github "sgrove")
     :asd none)
    (vana-templating (github "sgrove")
     :asd none)
    (vas-string-metrics (github "vsedach")
     :asd ("vas-string-metrics.asd"
           "test.vas-string-metrics.asd"))
    (vclos darcs "http://common-lisp.net/~crhodes/vclos")
    (vcs-tree (b9))
    (vectometry (github "xach"))
    ;; Update failed 2011-10-12
    ;; Repository problem on 2011-11-30.
    ;; Your configuration specifies to merge with the ref 'master' from the remote, but no
    ;; such ref was fetched.
    (vecto (xach))
    (verrazano darcs "http://common-lisp.net/project/fetter/darcs/verrazano"
     :asd ("verrazano.asd"
           "verrazano-runtime.asd"))
    (versioned-arrays (github "smithzvk")
     :asd ("versioned-arrays.asd"
           "versioned-arrays-test.asd"))
    (versioned-objects (github "madnificent"))
    (vivace-graph (github "kraison"))
    (vivace-graph-v2 (github "kraison")
     :asd ("vivace-graph-v2.asd"
           "vivace-graph-v2-test.asd"))
    (virgil (github "Lovesan")
     :asd ("virgil.asd"
           "virgil-test.asd"))
    (wclas git "http://cl-www.msi.co.jp/projects/wclas/wclas.git")
    (webfunk (github "gonzojive")
     :asd ("webfunk.asd"
           "contrib/metafunk/metafunk.asd"))
    (weblocks-dev (bitbucket "S11001001")
     :asd ("contrib/jwr/yui/weblocks-yui.asd"
           "contrib/s11001001/weblocks-s11.asd"
           "contrib/yarek/examples/employer-employee/employer-employee.asd"
           "contrib/yarek/examples/weblocks-demo-popover/weblocks-demo-popover.asd"
           "contrib/yarek/weblocks-yarek.asd"
           "examples/simple-blog/simple-blog.asd"
           "examples/weblocks-clsql-demo/weblocks-clsql-demo.asd"
           "examples/weblocks-demo/weblocks-demo.asd"
           "examples/weblocks-elephant-demo/weblocks-elephant-demo.asd"
           "scripts/new-app-templates/{APPNAME}.asd"
           "src/store/clsql/weblocks-clsql.asd"
           "src/store/elephant/weblocks-elephant.asd"
           "src/store/memory/weblocks-memory.asd"
           "src/store/prevalence/weblocks-prevalence.asd"
           "weblocks-scripts.asd"
           "weblocks-store-test.asd"
           "weblocks-test.asd"
           "weblocks.asd"))
    ;; (weblocks-stable hg "http://www.bitbucket.org/skypher/weblocks-stable"
    ;;  :asd ("contrib/jwr/yui/weblocks-yui.asd"
    ;;        "contrib/s11001001/weblocks-s11.asd"
    ;;        "contrib/yarek/examples/employer-employee/employer-employee.asd"
    ;;        "contrib/yarek/examples/weblocks-demo-popover/weblocks-demo-popover.asd"
    ;;        "contrib/yarek/weblocks-yarek.asd"
    ;;        "examples/simple-blog/simple-blog.asd"
    ;;        "examples/weblocks-clsql-demo/weblocks-clsql-demo.asd"
    ;;        "examples/weblocks-demo/weblocks-demo.asd"
    ;;        "examples/weblocks-elephant-demo/weblocks-elephant-demo.asd"
    ;;        "scripts/new-app-templates/{APPNAME}.asd"
    ;;        "src/store/clsql/weblocks-clsql.asd"
    ;;        "src/store/elephant/weblocks-elephant.asd"
    ;;        "src/store/memory/weblocks-memory.asd"
    ;;        "src/store/prevalence/weblocks-prevalence.asd"
    ;;        "weblocks-scripts.asd"
    ;;        "weblocks-store-test.asd"
    ;;        "weblocks-test.asd"
    ;;        "weblocks.asd"))
    (white-shadow (gitorious "white-shadow" "white-shadows"))
    (wiki-parser (github "archimag"))
    (wilbur darcs "http://www.crispylogics.com/opensource/repos/wilbur"
     :asd ("src/wilbur.asd"))
    (wispylisp darcs "http://common-lisp.net/project/wispylisp"
     :asd ("src/wispylisp.asd"))
    (wol (b9))
    ;; Used to be http://git.nklein.com/lisp/libs/woolly.git
    (woolly (github "nklein" "Woolly")
     :asd ("woolly.asd"
           "woolly-gl.asd"))
    (wormtrails (github "xach"))
    (wunderground (github "stassats"))
    (wuwei (github "mtravers"))
    (wxcl (sourceforge svn))
    (x.let-star (github "ks" "X.LET-STAR"))
    (x.fdatatypes (github "ks" "X.FDATATYPES"))
    (xarray (github "tpapp"))
    (xcl (github "gnooth") :asd none)
    ;; Also available at "http://common-lisp.net/project/xcvb/git/xcvb.git"
    ;; Which is best?
    (xcvb (github "fare")
     :asd ("examples/a2x/a2x-test.asd"
           "examples/example-2/xcvb-example-2.asd"
           "xcvb.asd"
           "xcvb-driver.asd"
           "t/xcvb-driver-test.asd"
           "t/xcvb-test.asd"
           "xcvb-bridge.asd"))
    ;; Repository problem on 2011-11-30.
    (xe2 (github "dto"))
    (xhtmlgen (github "hanshuebner"))
    (xlunit (b9))
    (xml-mop (github "gonzojive"))
    ;; XXXXXXXXXXXXXXXXXXXX: Why do I check this out?  Delete??
    (xmlisp (google-code svn)
     :asd none)
    (xmlutils (b9))
    (xong.blocky (github "dto")
     :asd none)
    (xptest (b9))
    (xuriella (repo-or-cz))
    (yacc darcs "http://www.pps.jussieu.fr/~jch/software/repos/cl-yacc")
    (yacc-ebnf (gitorious "cl-yacc-ebnf" "cl-yacc-ebnf")
     :asd ("src/yacc-ebnf.asd"))
    (yaclml darcs "http://common-lisp.net/project/bese/repos/yaclml")
    (yadd (google-code svn "cl-yadd"))
    (yashmup (github "sykopomp")
     :asd ("yashmup.asd"
           "util/sprite-checker/sprite-checker.asd"))
    (yason (github "hanshuebner"))
    (yotta-zoomer (repo-or-cz))
    (you (github "quek")
     :asd ("you.asd"
           "example/tt/you.example.tt.asd"
           "example/chat/you.example.chat.asd"
           "example/watch/you.example.watch.asd"
           "example/todo/you.example.todo.asd"
           "example/blog/you.example.blog.asd"))
    (zcdb (github "xach"))
    (zen (github "pyb")
     :asd none)
    (zip (clnet cvs))
    (zipper-1 (github "g000001"))
    (zl-compat (github "g000001"))
    (zlib (github "franzinc")
     :asd none)
    (zpb-aws (github "xach"))
    ;; Update failed 2011-10-12
    ;; Repository problem on 2011-11-30.
    ;; Your configuration specifies to merge with the ref 'master' from the remote, but no
    ;; such ref was fetched.
    (zpb-exif (xach))
    ;; Update failed 2011-10-12
    ;; Repository problem on 2011-11-30.
    ;; Your configuration specifies to merge with the ref 'master' from the remote, but no
    ;; such ref was fetched.
    (zpb-ttf (xach))
    ;; Update failed 2011-10-12
    ;; Repository problem on 2011-11-30.
    ;; Your configuration specifies to merge with the ref 'master' from the remote, but no
    ;; such ref was fetched.
    (zpng (xach))
    (zs3 (github "xach"))

    ;;; Repositories not related to Common Lisp.

    (dwarf git "http://git.androdna.com/lisp/dwarf" :asd none)
    ;; (emacs (savannah git) :asd none)
    (emacs-jabber (sourceforge cvs) :asd none)
    (git git "git://git.kernel.org/pub/scm/git/git.git" :asd none)
    (libsigsegv (savannah cvs) :asd none)
    (magit git "git://gitorious.org/magit/mainline.git" :asd none)
    (gnome-common git "git://git.gnome.org/gnome-common" :asd none)
    (gnome-terminal git "git://git.gnome.org/gnome-terminal" :asd none)
    (vte git "git://git.gnome.org/vte" :asd none)
    (wave-client-for-emacs (google-code hg) :asd none)
    ;; Repository problem on 2011-11-30.
    (wave-protocol (google-code hg) :asd none)
    )
  "Database of projects we are interested in cloning locally.")


;;;     Abbreviation functions for common code repositories


(defun b9 (project &optional (repository (string-downcase project)))
  "Repository specification abbreviation function for a git project hosted on
b9.com."
  ;; XXXX: git:// used to work.  Try it again soon.
  `(git ,(concat "http://git.b9.com/" repository ".git")))

(defun bitbucket (project user)
  "Repository specification abbreviation function for a Mercurial project hosted
on bitbucket.org."
  (let ((repository (string-downcase project)))
    `(hg ,(concat "https://bitbucket.org/" user "/" repository))))

(defun clnet (project scms
              &optional (name (string-downcase project)) (repository (string-downcase project)))
  "Repository specification abbreviation function for a project hosted on
common-lisp.net."
  (ecase scms
    ((cvs) `(cvs pserver anonymous t common-lisp.net ,(concat "/project/" name "/cvsroot")
                 :module ,repository))
    ((darcs) `(darcs ,(concat "http://common-lisp.net/project/" name "/darcs/" repository)))
    ((git) `(git ,(concat "git://common-lisp.net/projects/" name "/" repository ".git")))
    ((svn) `(svn ,(concat "svn://common-lisp.net/project/" name "/svn/trunk")))))

(defun melis (project &optional (repository (string-downcase project)))
  "Repository specification abbreviation function for a git project hosted by
Gabor Melis on quotenil.com."
  `(git ,(concat "http://quotenil.com/git/" repository ".git")))

(defun github (project user &optional (repository (string-downcase project)))
  "Repository specification abbreviation function for a git project hosted on
github.com."
  `(git ,(concat "git://github.com/" user "/" repository ".git")))

(defun gitorious (project
                  &optional (name (string-downcase project))
                            (repository (string-downcase project)))
  "Repository specification abbreviation function for a git project hosted on
gitorious.org."
  `(git ,(concat "git://gitorious.org/" name "/" repository ".git")))

(defun google-code (project scms &optional (repository (string-downcase project)))
  "Repository specification abbreviation function for a Subversion or Mercurial
repository hosted on code.google.com."
  (ecase scms
    ((hg) `(hg ,(concat "http://code.google.com/p/" repository)))
    ((svn) `(svn ,(concat "http://" repository ".googlecode.com/svn/trunk")))))

(defun harmon (project)
  "Repository specification abbreviation function for Cyrus Harmon's git
projects hosted on http://git.cyrusharmon.org/cgi-bin/gitweb.cgi"
  (let ((name (string-downcase project)))
    `(git ,(concat "git://cyrusharmon.org/pub/git/" name ".git"))))

(defun dwim-hu (project)
  "Repository specification abbreviation function for darcs projects hosted on
dwim.hu."
  (let ((name (string-downcase project)))
    `(darcs ,(concat "http://dwim.hu/darcs/" name))))

(defun repo-or-cz (project)
  "Repository specification abbreviation function for git projects hosted on
repo.or.cz."
  (let ((name (string-downcase project)))
    `(git ,(concat "git://repo.or.cz/" name ".git"))))

(defun savannah (project scms)
  "Repository specification abbreviation function for a project hosted on
savannah.gnu.org."
  (let ((name (string-downcase project)))
    (ecase scms
      ((bzr) `(bzr ,(concat "http://bzr.savannah.gnu.org/r/" name "/trunk")))
      ((cvs) `(cvs pserver anonymous nil cvs.savannah.gnu.org ,(concat "/sources/" name)
                   :module ,name))
      ((git) `(git ,(concat "git://git.savannah.gnu.org/" name ".git"))))))

(defun sourceforge (project scms &optional (repository (string-downcase project)))
  "Repository specification abbreviation function for a project hosted on
sourceforge.net."
  (ecase scms
    ((cvs) `(cvs pserver anonymous nil
                 ,(intern (string-upcase (concat repository ".cvs.sourceforge.net")))
                 ,(concat "/cvsroot/" repository)
                 :module ,repository))
    ((git) `(git ,(concat "git://" repository ".git.sourceforge.net/gitroot/" repository
                          "/" repository ".git")))
    ((svn) `(svn ,(concat "https://" repository ".svn.sourceforge.net/svnroot/" repository)))))

(defun xach (project)
  "Repository specification abbreviation function for Zach Beane's git projects
hosted on http://git.xach.com"
  (let ((name (string-downcase project)))
    `(git ,(concat "http://git.xach.com/" name ".git"))))


;;;     Parse repository specifications to create the repository database


(defun parse-repository-spec (spec)
  "Parse a repository specification.  The form of the repository entry is
either (NAME SCMS SCMS-ARGS MORE-ARGS) or (NAME (ABBREV-FUNC ABBREV-ARGS)
MORE-ARGS).  In the latter case, ABBREV-FUNC is invoked on (cons NAME
ABBREV-ARGS) to create SCMS and SCMS-ARGS."
  (ecase (type-of (second spec))
    ((symbol) spec)
    ((cons) (destructuring-bind (project (scms . arguments) &rest rest)
                spec
              `(,project ,@(apply scms (cons project arguments)) ,@rest)))))

(defun make-database (repository-specs)
  "Create the repository database from a list of repository specifications."
  (mapcar #'parse-repository-spec repository-specs))

(defparameter *database* (make-database +repository-specs+)  "Repository database")


;;;     Operations on the parsed repository database


(defun find-project (name)
  "Find the repository specification of the project.  Return two values, the
source code management system (SCMS) used by the project and the project
specification with the SCMS removed."
  (let ((project-spec (assoc name *database*)))
    (when project-spec
      (destructuring-bind (project scms &rest rest)
          project-spec
        (values scms `(,project ,@rest))))))

(defun operate (project operation)
  "Perform an operation on a project."
  (multiple-value-bind (scms arguments)
      (find-project project)
    (unless scms
      (error "unknown project"))
    ;; Lack of an asd specification means there's one asd file named after the project.
    (unless (member :asd arguments)
      (setf arguments (append arguments '(:asd project))))
    (apply scms (cons operation arguments))
    (values)))

(defun checkout (project)
  "Check out a project."
  (format t "~%checkout ~a~%" project)
  (operate project 'checkout))

(defun checkout-repositories (repository-specs)
  "Check cout all the projects listed in REPOSITORY-SPECS."
  (loop for (name . nil) in repository-specs do (checkout name))
  (values))

(defun checkout-all ()
  "Check out all the project repositories in the database."
  (checkout-repositories *database*))

(defun checkout-all-starting-with (repository-name)
  "Check out all the project repositories in the database starting with the
entry for REPOSITORY-NAME."
  (let ((start (position repository-name *database* :key #'first)))
    (checkout-repositories (subseq *database* start))))

(defun update (project)
  "Update a project."
  (format t "~%update ~a~%" project)
  (operate project 'update))

(defun update-repositories (repository-specs)
  "Update all the projects listed in REPOSITORY-SPECS."
  (loop for (name . nil) in repository-specs do (update name))
  (values))

(defun update-all ()
  "Update all the project repositories in the database."
  (update-repositories *database*))

(defun update-all-starting-with (repository-name)
  "Update all the project repositories in the database starting with the entry
for REPOSITORY-NAME."
  (let ((start (position repository-name *database* :key #'first)))
    (update-repositories (subseq *database* start))))

(defun change-directory (directory)
  #+ccl (setf (ccl:current-directory) directory)
  #+sbcl (sb-posix:chdir directory))

(defun current-directory ()
  #+ccl (ccl:current-directory)
  #+sbcl (sb-posix:getcwd))

(defmacro with-cwd (directory &body body)
  (let ((original-directory (gensym "original-directory"))
        (chdir-worked (gensym "chdir-worked")))
    `(let ((,original-directory (current-directory))
           (,chdir-worked nil))
       (unwind-protect
            (progn (change-directory ,directory)
                   (setf ,chdir-worked t)
                   ,@body)
         (when ,chdir-worked (change-directory ,original-directory))))))

(defun run (program args)
  #+ccl
  (let ((process (ccl:run-program program args :output *terminal-io* :wait t)))
    (multiple-value-bind (status exit-code)
        (ccl:external-process-status process)
      (assert (and (eq status :exited) (zerop exit-code)))))
  #+sbcl
  (let ((process (sb-ext:run-program program args :output *terminal-io* :search t :wait t)))
    (assert (zerop (sb-ext:process-exit-code process)))))

(defun link (project-directory asd-file-path)
  (let* ((link-target (concat project-directory "/" asd-file-path))
         (slash (position #\/ asd-file-path :from-end t))
         (asd-file (if slash
                       (subseq asd-file-path (1+ slash))
                       asd-file-path))
         (link (concat *systems-root* "/" asd-file)))
    (assert (probe-file link-target))
    (format t "Create link to ~a~%" link-target)
    (run "rm" `("-f" ,link))
    (run "ln" `("-s" ,link-target ,link))))

(defun create-asd-links (project-directory project asd-files)
  (cond ((eq asd-files 'none))
        ((eq asd-files 'project)
         (link project-directory (concat project ".asd")))
        ((listp asd-files)
         (dolist (asd-file asd-files)
           (link project-directory asd-file)))
        (t (error "bad :ASD specifier"))))


;;;     Source code management systems


(defun cvs (op name method user password host root &key asd (module (string-downcase name)))
  "Check out or update a CVS repository."
  (let* ((name (string-downcase name))
         (user (string-downcase user))
         (cvs-root
           (concat "-d" ":" (string-downcase method)
                   (case password
                     ((t) (concat ":" user))
                     ((nil) "")
                     (otherwise (concat ":" password)))
                   ":" user "@" (string-downcase host)
                   ":" root))
         (project-directory (concat *source-root* "/" name)))
    (ecase op
      ((checkout)
       (with-cwd *source-root*
         (run "cvs" `("-z3" ,cvs-root "co" ,module))
         (when (not (string= name module)) (run "mv" `(,module ,name)))))
      ((update)
       (with-cwd project-directory
         (run "cvs" `("-z3" ,cvs-root "up" "-A" "-P" "-d")))))
    (create-asd-links project-directory name asd)))

(defun bzr (op name url &key asd lightweight)
  "Check out or update a bazaar repository."
  (let* ((name (string-downcase name))
         (project-directory (concat *source-root* "/" name)))
    (ecase op
      ((checkout)
       (with-cwd *source-root*
         (run "bzr" `("checkout" "--verbose" ,@(when lightweight '("--lightweight")) ,url ,name))))
      ((update)
       (with-cwd project-directory
         (run "bzr" '("update" "--verbose")))))
    (create-asd-links project-directory name asd)))

(defun darcs (op name url &key asd)
  "Check out or update a darcs repository."
  (let* ((name (string-downcase name))
         (project-directory (concat *source-root* "/" name)))
    (ecase op
      ((checkout)
       (with-cwd *source-root*
         (run "darcs" `("get" ,url ,name))))
      ((update)
       (with-cwd project-directory
         ;; Sometimes a darcs pull fails because there's a leftover lock file from a previous
         ;; interrupted pull.  Should we try to remove the lock file, repository/_darcs/lock?
         (run "darcs" '("pull" "--all" "--verbose")))))
    (create-asd-links project-directory name asd)))

(defun git (op name url &key asd submodules)
  "Check out or update a Git repository.  When SUBMODULES is non-nil, check out
or update all submodules."
  (let* ((name (string-downcase name))
         (project-directory (concat *source-root* "/" name)))
    (ecase op
      ((checkout)
       (with-cwd *source-root*
         (run "git" `("clone" ,url ,name))))
      ((update)
       (with-cwd project-directory
         (run "git" '("pull")))))
    (when submodules
      (with-cwd project-directory
        (run "git" '("submodule" "init"))
        (run "git" '("submodule" "update"))))
    (create-asd-links project-directory name asd)))

(defun hg (op name url &key asd)
  "Check out or update a Mercurial repository."
  (let* ((name (string-downcase name))
         (project-directory (concat *source-root* "/" name)))
    (ecase op
      ((checkout)
       (with-cwd *source-root*
         (run "hg" `("clone" ,url ,name))))
      ((update)
       (with-cwd project-directory
         (run "hg" '("pull" "--update")))))
    (create-asd-links project-directory name asd)))

(defun svn (op name url &key asd)
  "Check out or update a Subversion repository."
  (let* ((name (string-downcase name))
         (project-directory (concat *source-root* "/" name)))
    (ecase op
      ((checkout)
       (with-cwd *source-root*
         (run "svn" `("checkout" ,url ,name))))
      ((update)
       (with-cwd project-directory
         (run "svn" '("update")))))
    (create-asd-links project-directory name asd)))
