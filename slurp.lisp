
;;;;    slurp.lisp


(in-package #:slurp)

(defparameter *source-root* "/local/software/source-trees"
  "Directory into which source code repositories are checked out.")

(defparameter *systems-root* "/local/software/systems"
  "Directory populated with symbolic links to repository ASDF files.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun concat (&rest args)
    (apply #'concatenate (cons 'string args))))


(defparameter +repositiory-specs+
  '((3b-swf (github "3b")
     :asd ("3b-swf-swc.asd"
           "3b-swf.asd"))
    (3bil (github "3b")
     :asd ("3b-swf-writer.asd"
           "avm2-asm.asd"
           "avm2-compile.asd"
           "avm2-lib.asd"
           "swf-writer-hack.asd"))
    (abcl svn "svn://common-lisp.net/project/armedbear/svn/trunk/abcl")
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
    (amazon-ecs (github "gonzojive"))
    (amd64-asm (google-code svn))
    (anaphora (clnet cvs "src"))
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
    (aquamacs cvs pserver anonymous nil cvs.aquamacs.org "/cvsroot/aquamacs"
     :asd none)
    (araneida-release darcs "http://common-lisp.net/project/araneida/araneida-release"
     :asd none)
    (araneida-testing darcs "http://common-lisp.net/project/araneida/araneida-testing"
     :asd ("araneida.asd"))
    ;; hanshueber has a fork of archive on github.
    (archive (github "froydnj"))
    (armish (clnet darcs))
    (arnesi_dev darcs "http://common-lisp.net/project/bese/repos/arnesi_dev"
     :asd ("arnesi.asd"))

    ;; XXXX: aromyxo may have been renamed to aromyxo-void
    ;; We need to support the following URL
    ;; git://gitorious.org/aromyxo/aromyxo-void.git
    ;;(aromyxo (gitorious))

    (array-operations (github "tpapp"))
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
           "autobench-web.asd"))
    (avl-tree (github "vy"))
    ;; XXXX Axiom has transitioned to git for version control, but which
    ;; repository is canonical?  The web site lists at least:
    ;;   git clone git://axiom.git.sourceforge.net/gitroot/axiom/axiom
    ;;   git clone git://git.savannah.nongnu.org/axiom.git
    ;;   git clone axiom@git.sv.nongnu.org:/srv/git/axiom.git
    ;; Figure out which is best.  Bill Daly's git repository on github and
    ;; the one on sourceforge are getting updates.
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
    (bknr svn "svn://svn.bknr.net/svn/trunk"
     :asd (
           ;; "thirdparty/cl-ftp/ftp.asd"
           ;; "thirdparty/cl-mime/cl-mime.asd"
           ;; "thirdparty/kmrcl-1.97/kmrcl.asd"
           ;; "thirdparty/kmrcl-1.97/kmrcl-tests.asd"
           ;; "thirdparty/cl-interpol/cl-interpol.asd"
           ;; "thirdparty/parse-number/parse-number.asd"
           ;; "thirdparty/parse-number/_darcs/pristine/parse-number.asd"
           ;; "thirdparty/atdoc/atdoc.asd"
           ;; "thirdparty/atdoc/example/blocks-world.asd"
           ;; "thirdparty/plexippus-xpath/xpath.asd"
           ;; "thirdparty/plexippus-xpath/_darcs/pristine/xpath.asd"
           ;; "thirdparty/babel/babel-streams.asd"
           ;; "thirdparty/babel/_darcs/pristine/babel-streams.asd"
           ;; "thirdparty/babel/_darcs/pristine/babel-tests.asd"
           ;; "thirdparty/babel/_darcs/pristine/babel.asd"
           ;; "thirdparty/babel/babel-tests.asd"
           ;; "thirdparty/babel/babel.asd"
           ;; "thirdparty/closure-html/closure-html.asd"
           ;; "thirdparty/cl-unicode/cl-unicode.asd"
           ;; "thirdparty/asdf/asdf.asd"
           ;; "thirdparty/asdf/test/try-recompiling-1.asd"
           ;; "thirdparty/asdf/test/test2b2.asd"
           ;; "thirdparty/asdf/test/test-modules-serial.asd"
           ;; "thirdparty/asdf/test/test-missing-lisp-file.asd"
           ;; "thirdparty/asdf/test/test-modules.asd"
           ;; "thirdparty/asdf/test/graveyard/test-preferences-system-1.asd"
           ;; "thirdparty/asdf/test/test-samedir-modules.asd"
           ;; "thirdparty/asdf/test/wild-module.asd"
           ;; "thirdparty/asdf/test/test2a.asd"
           ;; "thirdparty/asdf/test/test2.asd"
           ;; "thirdparty/asdf/test/test9-2.asd"
           ;; "thirdparty/asdf/test/test-force.asd"
           ;; "thirdparty/asdf/test/test-module-pathnames.asd"
           ;; "thirdparty/asdf/test/test1.asd"
           ;; "thirdparty/asdf/test/test-redundant-recompile.asd"
           ;; "thirdparty/asdf/test/test-nested-components-1.asd"
           ;; "thirdparty/asdf/test/test-module-excessive-depend.asd"
           ;; "thirdparty/asdf/test/test5.asd"
           ;; "thirdparty/asdf/test/test2b3.asd"
           ;; "thirdparty/asdf/test/test2b1.asd"
           ;; "thirdparty/asdf/test/test-module-depend.asd"
           ;; "thirdparty/asdf/test/try-reloading-1.asd"
           ;; "thirdparty/asdf/test/test9-1.asd"
           ;; "thirdparty/asdf/test/test-package.asd"
           ;; "thirdparty/asdf/test/test3.asd"
           ;; "thirdparty/asdf/test/file3-only.asd"
           ;; "thirdparty/asdf/test/static-and-serial.asd"
           ;; "thirdparty/unit-test/unit-test.asd"
           ;; "thirdparty/trivial-gray-streams/trivial-gray-streams.asd"
           ;; "thirdparty/zpb-ttf-0.7/zpb-ttf.asd"
           ;; "thirdparty/cl-yacc/yacc.asd"
           ;; "thirdparty/cl-yacc/_darcs/pristine/yacc.asd"
           ;; "thirdparty/md5/md5.asd"
           ;; "thirdparty/bordeaux-threads/bordeaux-threads.asd"
           ;; "thirdparty/lw-compat/lw-compat.asd"
           ;; "thirdparty/lw-compat/_darcs/current/lw-compat.asd"
           ;; "thirdparty/closure-common/closure-common.asd"
           ;; "thirdparty/trivial-backtrace/trivial-backtrace-test.asd"
           ;; "thirdparty/trivial-backtrace/_darcs/pristine/trivial-backtrace-test.asd"
           ;; "thirdparty/trivial-backtrace/_darcs/pristine/trivial-backtrace.asd"
           ;; "thirdparty/trivial-backtrace/trivial-backtrace.asd"
           ;; "thirdparty/cl-gd/cl-gd-test.asd"
           ;; "thirdparty/cl-gd/cl-gd.asd"
           ;; "thirdparty/cl-stm/cl-stm.asd"
           ;; "thirdparty/cl-stm/_darcs/pristine/cl-stm.asd"
           ;; "thirdparty/cl-xmlspam/cl-xmlspam.asd"
           ;; "thirdparty/zpb-exif/zpb-exif.asd"
           ;; "thirdparty/cl-ppcre/cl-ppcre-unicode.asd"
           ;; "thirdparty/cl-ppcre/cl-ppcre.asd"
           ;; "thirdparty/stem/stem.asd"
           ;; "thirdparty/chtml/closure-html.asd"
           ;; "thirdparty/stefil/_darcs/pristine/stefil.asd"
           ;; "thirdparty/stefil/stefil.asd"
           ;; "thirdparty/trivial-garbage/trivial-garbage.asd"
           ;; "thirdparty/trivial-garbage/_darcs/pristine/trivial-garbage.asd"
           ;; "thirdparty/cl-who/cl-who.asd"
           ;; "thirdparty/iterate/iterate.asd"
           ;; "thirdparty/iterate/_darcs/pristine/iterate.asd"
           ;; "thirdparty/cxml/cxml.asd"
           ;; "thirdparty/metabang-bind/metabang-bind-test.asd"
           ;; "thirdparty/metabang-bind/metabang-bind.asd"
           ;; "thirdparty/puri/puri.asd"
           ;; "thirdparty/slime/swank.asd"
           ;; "thirdparty/arnesi/arnesi.asd"
           ;; "thirdparty/alexandria/alexandria.asd"
           ;; "thirdparty/alexandria/alexandria-tests.asd"
           ;; "thirdparty/cl-store_0.8.4/cl-store.asd"
           ;; "thirdparty/pg/pg.asd"
           ;; "thirdparty/cl-pdf/cl-pdf-parser.asd"
           ;; "thirdparty/cl-pdf/cl-pdf.asd"
           ;; "thirdparty/cl-fad/cl-fad.asd"
           ;; "thirdparty/cl-webdav/cl-webdav.asd"
           ;; "thirdparty/drakma/drakma.asd"
           ;; "thirdparty/parenscript/parenscript.asd"
           ;; "thirdparty/vecto-1.0.2/vecto.asd"
           ;; "thirdparty/rt-20040621/rt.asd"
           ;; "thirdparty/usocket/usocket.asd"
           ;; "thirdparty/usocket/usocket-test.asd"
           ;; "thirdparty/uffi/uffi.asd"
           ;; "thirdparty/uffi/uffi-tests.asd"
           ;; "thirdparty/asdf-system-connections/asdf-system-connections.asd"
           ;; "thirdparty/fiveam/_darcs/pristine/fiveam.asd"
           ;; "thirdparty/fiveam/fiveam.asd"
           ;; "thirdparty/salza-png-1.0.1/salza-png.asd"
           ;; "thirdparty/trivial-features/_darcs/pristine/trivial-features-tests.asd"
           ;; "thirdparty/trivial-features/_darcs/pristine/trivial-features.asd"
           ;; "thirdparty/trivial-features/trivial-features-tests.asd"
           ;; "thirdparty/trivial-features/trivial-features.asd"
           ;; "thirdparty/rfc2388/rfc2388.asd"
           ;; "thirdparty/screamer/screamer.asd"
           ;; "thirdparty/hunchentoot/hunchentoot.asd"
           ;; "thirdparty/flexi-streams/flexi-streams.asd"
           ;; "thirdparty/cl-paypal/cl-paypal.asd"
           ;; "thirdparty/cffi/cffi-tests.asd"
           ;; "thirdparty/cffi/cffi.asd"
           ;; "thirdparty/cffi/cffi-grovel.asd"
           ;; "thirdparty/cffi/uffi-compat/uffi.asd"
           ;; "thirdparty/cffi/cffi-uffi-compat.asd"
           ;; "thirdparty/cffi/cffi-examples.asd"
           ;; "thirdparty/cl-smtp/cl-smtp.asd"
           ;; "thirdparty/salza-0.7.4/salza.asd"
           ;; "thirdparty/cl-vectors-0.1.3/cl-vectors.asd"
           ;; "thirdparty/cl-vectors-0.1.3/cl-paths-ttf.asd"
           ;; "thirdparty/cl-vectors-0.1.3/cl-paths.asd"
           ;; "thirdparty/cl-vectors-0.1.3/cl-aa-misc.asd"
           ;; "thirdparty/cl-vectors-0.1.3/cl-aa.asd"
           ;; "thirdparty/cxml-stp/cxml-stp.asd"
           ;; "thirdparty/trivial-https/trivial-https.asd"
           ;; "thirdparty/xuriella/xuriella.asd"
           ;; "thirdparty/split-sequence/split-sequence.asd"
           ;; "thirdparty/trivial-sockets/trivial-sockets.asd"
           ;; "thirdparty/cl-ssl/cl-ssl/cl-ssl.asd"
           ;; "thirdparty/closer-mop/_darcs/current/closer-mop.asd"
           ;; "thirdparty/closer-mop/closer-mop.asd"
           ;; "thirdparty/anaphora-0.9.3/anaphora.asd"
           ;; "thirdparty/cl-base64/cl-base64.asd"
           ;; "thirdparty/cybertiggyr-time/cybertiggyr-time.asd"
           ;; "thirdparty/trivial-utf-8/trivial-utf-8.asd"
           ;; "thirdparty/trivial-utf-8/_darcs/pristine/trivial-utf-8.asd"
           ;; "thirdparty/cl-qprint/cl-qprint.asd"
           ;; "thirdparty/chunga/chunga.asd"
           ;; "thirdparty/documentation-template/documentation-template.asd"
           ;; "thirdparty/defclass-star/defclass-star.asd"
           ;; "thirdparty/defclass-star/_darcs/pristine/defclass-star.asd"
           ;; "thirdparty/cl+ssl/cl+ssl.asd"
           ;; "thirdparty/iolib/examples/iolib.examples.asd"
           ;; "thirdparty/iolib/src/iolib.asd"
           ;; "thirdparty/iolib/src/iolib.base.asd"
           ;; "thirdparty/iolib/src/iolib.multiplex.asd"
           ;; "thirdparty/iolib/src/iolib.trivial-sockets.asd"
           ;; "thirdparty/iolib/src/iolib.streams.asd"
           ;; "thirdparty/iolib/src/iolib.pathnames.asd"
           ;; "thirdparty/iolib/src/iolib.os.asd"
           ;; "thirdparty/iolib/src/iolib.syscalls.asd"
           ;; "thirdparty/iolib/src/iolib.sockets.asd"
           ;; "thirdparty/iolib/src/iolib.zstreams.asd"
           ;; "thirdparty/iolib/tests/iolib-tests.asd"
           ;; "thirdparty/ironclad/ironclad.asd"
           "libraries/yason/yason.asd"
           "libraries/xhtmlgen/xhtmlgen.asd"
           "libraries/clixdoc/clixdoc.asd"
           "projects/hello-web/src/hello-web.asd"
           "projects/bos/m2/bos.m2.asd"
           "projects/bos/web/bos.web.asd"
           "projects/bos/test/bos.test.asd"
           "projects/scrabble/src/scrabble.asd"
           "projects/album-maker/src/album-maker.asd"
           "projects/lisp-ecoop/src/lisp-ecoop.asd"
           "projects/quickhoney/src/quickhoney.asd"
           "projects/unmaintained/saugnapf/src/saugnapf.asd"
           "projects/unmaintained/raw-data/mcp/mcp.asd"
           "projects/unmaintained/eboy/src/eboy.asd"
           "projects/unmaintained/gpn/gpn.asd"
           "projects/poll-postbank/poll-postbank.asd"
           "projects/mah-jongg/src/mah-jongg.asd"
           "projects/bknr-website/src/bknr.website.asd"))
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
    (blackthorn-engine (google-code hg)
     :asd ("blackthorn.asd"
           "thopter.asd"
           "blackthorn-collision-test.asd"
           "blackthorn-test.asd"
           "bunnyslayer.asd"
           "blackthorn-stress-test.asd"))
    (bordeaux-fft (github "ahefner"))
    ;; http://common-lisp.net/project/bordeaux-threads
    (bordeaux-threads (clnet git)
     :asd ("bordeaux-threads.asd"
           "bordeaux-threads-test.asd"))
    (bratwurst (github "sabetts"))
    (buclet (github "aerique"))
    (buildapp (github "xach"))
    (bytemap git "http://common-lisp.net/projects/bytemap/bytemap.git"
     :asd ("bytemap-test.asd"
           "bytemap.asd"))
    (caleb svn "svn://common-lisp.net/project/caleb/svn")
    ;; XXXX: Should work soon.
    ;; (categories git "git://codebasehq.com/bywicket/xg/categories.git"
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
           ; "cl-magick/cl-magick.asd"
           ; "cl-openal/cl-openal.asd"
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
    (ch-asdf (harmon))
    (ch-image (harmon)
     :asd ("ch-image.asd"
           "ch-image-doc.asd"
           "ch-image-test.asd"))
    (ch-util (harmon)
     :asd ("ch-util.asd"
           "ch-util-test.asd"))
    (chanl (github "sykopomp"))
    (chillax (github "sykopomp")
     :asd ("chillax.view-server.asd"
           "chillax.asd"
           "chillax.core.asd"
           "chillax.yason.asd"))
    (chipz (github "froydnj"))
    (chronicity (github "chaitanyagupta")
     :asd ("chronicity.asd"
           "chronicity-test.asd"))
    (chunga darcs "http://common-lisp.net/~loliveira/ediware/chunga")
    (cl+ssl cvs pserver anonymous t common-lisp.net "/project/cl-plus-ssl/cvsroot")
    (cl-2d (github "tpapp"))
    (cl-base64 git "git://git.b9.com/cl-base64.git")
    (cl-beanstalk (github "antifuchs"))
    (cl-bench (clnet svn)
     :asd none)
    (cl-berkeley-db (clnet darcs)
     :asd ("src/cl-berkeley-db.asd"))
    (cl-bio (harmon)
     :asd ("cl-bio-align.asd"
           "cl-bio-doc.asd"
           "cl-bio-entrez-doc.asd"
           "cl-bio-entrez-test.asd"
           "cl-bio-entrez.asd"
           "cl-bio-rucksack.asd"
           "cl-bio-taxonomy.asd"
           "cl-bio-test.asd"
           "cl-bio.asd"))
    (cl-blockfort (github "gonzojive"))
    (cl-blog svn "svn://unmutual.info/cl-blog/trunk/cl-blog")
    (cl-btree (github "gonzojive")
     :asd ("btree/btree.asd"))
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
    (cl-elf (repo-or-cz))
    ;; Checking out this repository prints:
    ;; This repo is OBSOLETE!
    ;; The new darcs2 repo is available at http://dwim.hu
    ;; XXXX: remove this repository and cl-dwim-old
    ;(cl-dwim (clnet darcs)
    ; :asd ("dwim.asd" "dwim-meta-model-test.asd"))

    ;; XXXXXXXXXXXXXXXXXXXX last update failed:
    ;; svn: Can't connect to host 'slimy.com': Connection refused
    ;(cl-e svn "svn://slimy.com/cl-e/cl-e/trunk/"
    ; :asd ("e-on-cl.asd"))
    (cl-fad darcs "http://common-lisp.net/~loliveira/ediware/cl-fad")
    (cl-fft git "http://git.nklein.com/lisp/libs/fft.git"
     :asd ("fft.asd"
           "pfft.asd"))
    (cl-fluiddb (github "hdurer")
     :asd ("cl-fluiddb.asd"
           "cl-fluiddb-test.asd"))
    (cl-frame (github "dto")
     :asd none)
    (cl-future (github "jpalmucci"))
    (cl-gambol (google-code svn)
     :asd "gambol.asd")
    (cl-gd darcs "http://common-lisp.net/~loliveira/ediware/cl-gd"
     :asd ("cl-gd.asd"
           "cl-gd-test.asd"))
    (cl-genomic (github "keithj")
     :asd ("cl-genomic.asd"
           "cl-genomic-test.asd"))
    (cl-geometry (github "Ramarren")
     :asd ("cl-geometry.asd"
           "cl-geometry-tests.asd"))
    (cl-gordon (sourceforge svn)
     :asd ("gordon/gordon.asd"
           ;; "gordon-branches/gordon-clos/gordon.asd"
           "torta/torta.asd"
           "ttf-flash/ttf-flash.asd"))
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
    (cl-imagemagick (github "franzinc")
     :asd none)
    (cl-interpol darcs "http://common-lisp.net/~loliveira/ediware/cl-interpol")
    (cl-irc (clnet svn)
     :asd ("cl-irc.asd"
           "example/cliki-bot.asd"
           "test/cl-irc-test.asd"))
    (cl-irregsexp (clnet git)
     :asd ("cl-irregsexp-test.asd"
           "cl-irregsexp.asd"))
    (cl-jpeg (clnet cvs "cljl"))
    (cl-json (clnet darcs))
    (cl-kyoto-cabinet (github "kraison"))
    (cl-l10n (clnet darcs))
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
    (cl-magick (clnet cvs))
    (cl-markdown darcs "http://common-lisp.net/project/cl-markdown"
     :asd ("cl-markdown-comparisons.asd"
           "cl-markdown-test.asd"
           "cl-markdown.asd"))
    (cl-mathstats darcs "http://common-lisp.net/project/cl-mathstats"
     :asd ("cl-mathstats.asd"
           "cl-mathstats-test.asd"))
    (cl-menusystem (clnet cvs))
    (cl-migrations (clnet darcs))
    (cl-mill (google-code svn)
     :asd ("gcode.asd"))
    (cl-mongrel2 (github "vseloved"))

; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;    (mpeg (clnet cvs "cl-mp3-parse" "cl-mp3-parse") :asd none)

    (cl-mpi (google-code svn)
     :asd ("cl-mpi.asd"
           "par-eval.asd"))
    (cl-mssql (github "archimag")
     :asd ("mssql.asd"))
    (cl-ncurses (clnet svn))
    (cl-net-snmp (sourceforge svn)
     :asd ("asn.1/trunk/asn.1-dev.asd"
           "asn.1/trunk/asn.1.asd"
           "contrib/msi/cffi-udp/cffi-udp.asd"
           "contrib/msi/snmp-nonblocking/snmp-nonblocking.asd"
           "ipmi/trunk/ipmi.asd"
           "ldap/trunk/ldap.asd"
           "ldap/trunk/trivial-ldap-0.71/trivial-ldap.asd"
           "lispworks-udp/trunk/lispworks-udp.asd"
           "snmp/trunk/snmp-base.asd"
           "snmp/trunk/snmp-client.asd"
           "snmp/trunk/snmp-dev.asd"
           "snmp/trunk/snmp-mib.asd"
           "snmp/trunk/snmp-server.asd"
           "snmp/trunk/snmp-test.asd"
           "snmp/trunk/snmp-ui.asd"
           "snmp/trunk/snmp.asd"
           "usocket-udp/trunk/usocket-udp.asd"))
    (cl-notify (repo-or-cz))
    (cl-numlib (github "tpapp"))
    (cl-num-utils (github "tpapp")
     :asd ("cl-num-utils.asd"
           "cl-num-utils-tests.asd"))
    (cl-oauth (github "skypher"))
    (cl-objc (clnet darcs))
    (cl-openal (github "sykopomp")
     :asd ("cl-openal.asd"
           "cl-openal-examples.asd"))
    ;; XXXXXXXXXX: There is a cl-opengl project on github.  Is it a better version?
    ;;(cl-opengl (github "3b") :asd none)
    (cl-opengl (clnet darcs)
     :asd ("cl-glu.asd"
           "cl-glut-examples.asd"
           "cl-glut.asd"
           "cl-opengl.asd"))
    (cl-openid (clnet darcs))
    (cl-parsec (github "vseloved"))
    (cl-parser-combinators (github "Ramarren")
     :asd ("parser-combinators.asd"
           "parser-combinators-tests.asd"))
    (cl-pdf svn "http://www.fractalconcept.com:8000/public/open-source/cl-pdf"
     :asd ("cl-pdf.asd"
           "cl-pdf-parser.asd"
           "salza/salza.asd"))
    (cl-peg darcs "http://subvert-the-dominant-paradigm.net/repos/cl-peg")
    (cl-photo git "git://git.b9.com/cl-photo.git"
     :asd ("cl-photo.asd"
           "cl-photo-tests.asd"))
    (cl-ppcre darcs "http://common-lisp.net/~loliveira/ediware/cl-ppcre"
     :asd ("cl-ppcre.asd"
           "cl-ppcre-test.asd"
           "cl-ppcre-unicode.asd"))
    (cl-prevalence (clnet cvs))
    ;; (clpython cvs pserver cvspublic t cvspublic.franz.com "/cvs-public"
    ;;  :asd ("clpython-test.asd"))
    (cl-prolog (github "keithj")
     :asd ("cl-prolog.asd"
           "cl-prolog-test.asd"
           "cl-swi.asd"
           "cl-swi-client.asd"))
    (clpython (github "franzinc" "cl-python"))
    (cl-randist (github "lvaruzza"))
    (cl-random (github "tpapp"))
    (cl-rdfxml svn "http://svn.cs.rpi.edu/svn/tayloj/cl-rdfxml")
    ;; XXXX: This version of cl-rdfxml may be better.  Active development?
    ;; (cl-rdfxml (github "turbo24prg") :asd none)
    (cl-redis (github "vseloved"))
    (cl-rogue (google-code svn))
    (cl-routes (github "archimag")
     :asd ("routes.asd"
           "routes-test.asd"))
    (cl-sails (github "gonzojive"))
    (cl-sam (github "keithj")
     :asd ("cl-sam.asd"
           "cl-sam-test.asd"))
    (cl-selenium (clnet cvs)
     :asd ("selenium.asd"))
    (cl-skip-list (github "kraison"))
    (cl-smtp (clnet cvs))
    (cl-spidermonkey (github "gonzojive"))
    (cl-sqlite (repo-or-cz)
     :asd ("sqlite-tests.asd"
           "sqlite.asd"))
    (cl-starcraft-proxybot (github "aerique"))
    (cl-stm darcs "http://common-lisp.net/project/cl-stm")
    (cl-store (clnet cvs))
    (cl-strings (google-code svn)
     :asd ("cl-strings.asd"
           "cl-strings-tests.asd"))
    (cl-svg (google-code svn))
    ;; http://common-lisp.net/projects/cl-machine-learning/git/cl-svm/.git
    ;; Look for a newer cl-swm repository at github.com/gonzojive.
    (cl-svm git "http://common-lisp.net/project/suave/git/cl-svm/.git")
    (cl-taint darcs "http://www.common-lisp.net/project/cl-taint/cl-taint-release")
    (cl-tc (github "unya")
     :asd ("cl-tc.asd"
           "tokyocabinet.asd"
           "tokyodystopia.asd"
           "tokyotyrant.asd"))
    (cl-telnetd (clnet cvs)
     :asd none)
    (cl-tidy (github "gonzojive"))
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
    (cl-unification (clnet cvs)
     :asd ("cl-unification.asd"
           "cl-unification-lib.asd"))
    (cl-uri (clnet darcs)
     :asd ("src/cl-uri.asd"))
    (cl-uri-templates (github "billitch")
     :asd ("cl-uri-templates.test.asd"
           "cl-uri-templates.asd"))
    (cl-utilities (clnet cvs))
    (cl-variates (clnet darcs)
     :asd ("cl-variates.asd"
           "cl-variates-test.asd"))
    (cl-web-crawler (google-code svn))
    (cl-who darcs "http://common-lisp.net/~loliveira/ediware/cl-who")
    (cl-whois (github "billitch"))
    (cl-x86-asm (repo-or-cz))
    (cl-xmpp (clnet cvs)
     :asd ("cl-xmpp-sasl.asd"
           "cl-xmpp-tls.asd"
           "cl-xmpp.asd"
           "test/cl-xmpp-test.asd"))
    ;; XXXX: Remove the "cl-" from the name of this project.
    (cl-yacc darcs "http://www.pps.jussieu.fr/~jch/software/repos/cl-yacc"
     :asd ("yacc.asd"))
    ;; XXXX: Remove the "cl-" from the name of this project.
    (cl-yacc-ebnf (gitorious)
     :asd ("src/yacc-ebnf.asd"))
    (cl-zmq (repo-or-cz)
     :asd ("zeromq.asd"))
    (claw (clnet svn)
     :asd ("main/claw-as/claw-as.asd"
           "main/claw-demo/claw-demo.asd"
           "main/claw-html.dojo/claw-html.dojo.asd"
           "main/claw-html/claw-html.asd"
           "main/claw.i18n/claw.i18n.asd"
           "main/connectors/claw-hunchentoot-connector/claw-hunchentoot-connector.asd"
           "site/claw-site.asd"))
    (clazy (clnet cvs))
    (clbuild (gitorious)
     :asd none)
    (clem (harmon)
     :asd ("clem.asd"
           "clem-benchmark.asd"
           "clem-doc.asd"
           "clem-test.asd"))
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
    (clim-desktop (clnet cvs)
     :asd ("clim-desktop.asd"
           "clim-desktop-minimal.asd"))
    (climacs (clnet cvs))
    (climc (google-code svn))
    (clisp (sourceforge cvs)
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
    (clon (github "dto"))
    (clonsigna (clnet git))
    ;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ;; Same project name as other clon.  Figure out a solution.
    ;; Need different directory name and renaming of asd file links.
    ;; (clon (melis) :asd none)
    (closer-mop darcs "http://common-lisp.net/project/closer/repos/closer-mop")
    (closure (clnet cvs))
    (closure-common (repo-or-cz))
    (closure-html (repo-or-cz))
    (clouchdb (clnet cvs)
     :asd ("src/clouchdb.asd"
           "src/clouchdb-examples.asd"
           "src/clouchdb-tests.asd"))
    (clqr (repo-or-cz)
     :asd none)
    (clsql git "git://git.b9.com/clsql.git"
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
    (clsr (harmon)
     :asd ("clsr.asd"
           "clsr-doc.asd"
           "clsr-gen.asd"
           "clsr-test.asd"))
    (cluck git "git://git.b9.com/cluck.git")
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
    (cmucl (clnet cvs "src")
     :asd none)
    (com.gigamonkeys.binary-data (github "gigamonkey" "binary-data"))
    (com.gigamonkeys.macro-utilities (github "gigamonkey" "macro-utilities"))
    (com.gigamonkeys.pathnames (github "gigamonkey" "pathnames"))
    (com.gigamonkeys.spam (github "gigamonkey" "spam"))
    (com.gigamonkeys.utilities (github "gigamonkey" "utilities"))
    (common-lisp-stat (github "blindglobe")
     :submodules t
     :asd none)
    (common-worm (github "sykopomp"))
    ;; commonqt-OLD comes from repo.or.cz
    ;; (commonqt git "git://repo.or.cz/commonqt.git" ...)
    ;; Checkout of that repository fails, so I switched to lichteblau's
    ;; XXXX: Which is the canonical source?
    (commonqt git "http://www.lichteblau.com/git/commonqt.git"
     :asd ("qt.asd"
           "qt-repl.asd"
           "qt-tutorial.asd"))
    (conium (gitorious))
    (contextl darcs "http://common-lisp.net/project/closer/repos/contextl")
    (css-lite (github "paddymul"))
    (cusp svn "http://cusp.googlecode.com/svn"  ; missing /trunk on URI
     :asd none)
    (cxml (repo-or-cz))
    (cxml-rpc (github "antifuchs"))
    (date-utils (github "vseloved")
     :asd none)
    (deflate (github "pmai" "Deflate"))
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
    (discworld (github "cddr"))
    (dispatch (github "sellout" "cl-dispatch"))
    (docudown darcs "http://common-lisp.net/project/docudown"
     :asd ("docudown.asd"
           "docudown-test.asd"))
    (drakma darcs "http://common-lisp.net/~loliveira/ediware/drakma")
    (dynamic-classes darcs "http://common-lisp.net/project/dynamic-classes"
     :asd ("dynamic-classes.asd"
           "dynamic-classes-test.asd"))
    (eager-future (github "vsedach" "Eager-Future"))
    (ecl git "http://ecls.sourceforge.net/git/ecl/.git"
     :asd none)
    (ecl-doc git "http://ecls.sourceforge.net/git/ecl-doc/.git"
     :asd none)
    (ecl-test git "http://ecls.sourceforge.net/git/ecl-test/.git"
     :asd none)
    (elephant (clnet darcs "elephant-1.0")
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
    (epigraph (github "slyrus")
     :asd ("epigraph.asd"
           "epigraph-doc.asd"
           "epigraph-test.asd"))
    (esa cvs pserver anonymous t common-lisp.net "/project/climacs/cvsroot")
    (esrap (github "nikodemus")
     :asd none)
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
           ;"expresso/trunk/shell/uffi/uffi.asd"
           ;"expresso/trunk/shell/uffi/uffi-tests.asd"
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
    (femlisp (savannah cvs))
    (ffa (github "tpapp"))
    (filtered-functions darcs "http://common-lisp.net/project/closer/repos/filtered-functions")
    (fiveam darcs "http://common-lisp.net/project/bese/repos/fiveam")
    (flexi-streams darcs "http://common-lisp.net/~loliveira/ediware/flexi-streams")
    (flexichain (clnet cvs)
     :asd ("flexichain-doc.asd"
           "flexichain-test.asd"
           "flexichain.asd"))
    (folio git "git://codebasehq.com/bywicket/xg/folio.git"
     :asd ("as/as.asd"
           "collections/collections.asd"
           "lib/misc-extensions_1.2.0/misc-extensions.asd"
           ;; XXXX: Conflict with fset project.
           ;; "lib/fset_1.2.2/fset.asd"
           "folio.asd"))
    (freeimage (github "BradWBeer" "CL-FreeImage"))
    (fricas (sourceforge svn)
     :asd none)
    (fset (clnet svn))
    (fsbv (repo-or-cz))
    (fsvd (melis))
    (ftd (clnet darcs))
    (garnet cvs pserver anonymous nil garnetlisp.cvs.sourceforge.net "/cvsroot/garnetlisp"
     :asd none)
    (gcl (savannah cvs)
     :asd none)
    (gcc-xml-ffi (harmon)
     :asd ("gcc-xml-ffi.asd"
           "gcc-xml-ffi-test.asd"))
    (glaw (github "patzy")
     :asd ("glaw-sdl.asd"
           "glaw-imago.asd"
           "glaw.asd"
           "glaw-examples.asd"))
    (glitter (github "froydnj"))
    (glop (github "patzy")
     :asd ("glop.asd"
           "glop-test.asd"))
    (google-protobuf (github "brown")
     :asd none)
    (getopt git "git://git.b9.com/getopt.git")
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
    (gsll (repo-or-cz)
     :asd ("gsll.asd"
           "gsll-tests.asd"))
    (hemlock (gitorious)
     :asd ("hemlock.base.asd"
           "hemlock.qt.asd"
           "hemlock.clx.asd"
           "hemlock.tty.asd"))
    (hob git "http://marijn.haverbeke.nl/git/hob")
    (html-entities (google-code svn))
    (html-template darcs "http://common-lisp.net/~loliveira/ediware/html-template")
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
    ;; I use swank.asd from the slime project.
    (hu.dwim.slime (dwim-hu)
     :asd none)
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
    ;; XXXXXXXXXXXXXXXXXXXX Planet Lisp says there were recent changes, but
    ;; update didn't change anything.  Do I have the right repository?
    (hunchentoot darcs "http://common-lisp.net/~loliveira/ediware/hunchentoot"
     :asd ("hunchentoot.asd"
           "hunchentoot-test.asd"))
    (hunchentoot-auth (harmon)
     :asd ("hunchentoot-auth.asd"
           "hunchentoot-auth-test.asd"))
    (hunchentoot-cgi (harmon))
    (hunchentoot-vhost (harmon))
    (hyperobject git "git://git.b9.com/hyperobject.git"
     :asd ("hyperobject.asd"
           "hyperobject-tests.asd"))
    (ia-x86 cvs pserver anonymous t common-lisp.net "/project/movitz/cvsroot")
    (ieee-floats (clnet darcs))
    (imago (clnet cvs)
     :asd ("src/imago.asd"))
    (imap (github "franzinc")
     :asd none)
    (incf-cl (github "jmbr"))
    ;; XXXX: what about iolib/iolib and iolib/static-vectors on gitorious??
    ;; fe[nl]ix says http://gitorious.org/iolib/static-vectors/ is canonical
    (iolib (repo-or-cz)
     :asd ("examples/iolib.examples.asd"
           "src/iolib.asd"
           "src/iolib.base.asd"
           "src/iolib.multiplex.asd"
           "src/iolib.os.asd"
           "src/iolib.pathnames.asd"
           "src/iolib.sockets.asd"
           "src/iolib.streams.asd"
           "src/iolib.syscalls.asd"
           "src/iolib.trivial-sockets.asd"
           "tests/iolib-tests.asd"))
    (irc-logger git "git://git.b9.com/irc-logger.git")
    (ironclad (github "froydnj"))
    (iterate (clnet darcs))
    (j cvs pserver anonymous nil armedbear-j.cvs.sourceforge.net "/cvsroot/armedbear-j"
     :asd none)
    (jpegmeta (google-code svn)
     :asd ("jpegmeta.asd"
           ;; "binary-data/com.gigamonkeys.binary-data.asd"
           ;; "macro-utilities/com.gigamonkeys.macro-utilities.asd"
           ))
    (js (github "akapav"))
    ;; XXXX: Is this an early version of parse-js ?
    (js-parser (github "gonzojive")
     :asd ("js-parser-tests.asd"
           "js-parser.asd"))
    (js-toolkit (github "vsedach"))
    (jwacs darcs "http://chumsley.org/jwacs/unstable"
     :asd ("jwacs.asd"
           "jwacs-tests.asd"))
    (kilns  (github "sellout" "Kilns"))
    (kmrcl git "git://git.b9.com/kmrcl.git"
     :asd ("kmrcl.asd"
           "kmrcl-tests.asd"))
    (kpax darcs "http://www.beta9.be/darcs/kpax"
     :asd ("kpax-core.asd"
           "kpax-examples.asd"
           "kpax-mod-lisp.asd"
           "kpax-paserve.asd"
           "kpax-s-http-server.asd"
           "kpax.asd"))
    (lassie (melis))
    (latex-table (github "tpapp"))
    ;; XXXX: This user has disappeared on github.
    ;; (lexer (github "turbo24prg"))
    (liards (clnet darcs))
    (lice (repo-or-cz)
     :asd ("src/lice.asd"))
    (lift (github "gwkkwg")
     :asd ("lift.asd"
           "lift-test.asd"
           "lift-documentation.asd"))
    (lisa (sourceforge cvs))
    (lisp-matrix (github "blindglobe"))
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
    (lispdev hg "http://bitbucket.org/skolos/lispdev"
     :asd none)
    ;; XXXX: maybe change project name to lisppaste
    (lisppaste2 cvs pserver anonymous t common-lisp.net "/project/lisppaste/cvsroot"
     :asd ("lisppaste.asd"))
    (lla (github "tpapp"))
    (llvm  (github "sellout" "CL-LLVM"))
    (lml git "git://git.b9.com/lml.git"
     :asd ("lml.asd"
           "lml-tests.asd"))
    (lml2 git "git://git.b9.com/lml2.git"
     :asd ("lml2.asd"
           "lml2-tests.asd"))
    (local-time (clnet darcs))
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
    (lsw2 (google-code svn)
     :asd none)
    (lw-compat darcs "http://common-lisp.net/project/closer/repos/lw-compat")
    (macrophp (github "valeryz" "MacroPHP"))
    (maild (github "franzinc")
     :asd none)
    ;; Also available from git://github.com/ilitirit/manardb.git
    ;; Maybe that's the real master?  double check other msi projects
    (manardb git "http://cl-www.msi.co.jp/projects/manardb/manardb.git"
     :asd ("manardb.asd"
           "manardb-test.asd"))
    (math-extensions (github "sellout"))
    (matlisp cvs pserver anonymous nil matlisp.cvs.sourceforge.net "/cvsroot/matlisp"
     :asd none)
    (maxima (sourceforge cvs)
     :asd ("src/maxima.asd"))
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
    (mcpixel (github "ahefner" "McPixel"))
    (md5 git "git://git.b9.com/md5.git")
    (mel-base darcs "http://www.crispylogics.com/opensource/repos/mel-base")
    ;; XXXX: mel-base-old got a bunch of updates on jan 18 2010 that were
    ;; old ... jan to nov of 2009
    (mel-base-old darcs "http://common-lisp.net/project/mel-base/darcs/mel-base"
     :asd none)
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
    (mgl (melis)
     :asd ("mgl-test.asd"
           "mgl.asd"
           "mgl-example.asd"
           "mgl-visuals.asd"))
    (mixalot (github "ahefner")
     :asd ("mixalot.asd"
           "mixalot-mp3.asd"
           "mpg123-ffi.asd"))
    ;; XXXXXXXXXX empty repository ?? ask Gabor about it
    ;; (micmac (melis) :asd none)
    (mod_lisp svn "http://www.fractalconcept.com:8000/public/open-source/mod_lisp"
     :asd none)
    (modlisp git "git://git.b9.com/cl-modlisp.git")
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
    (movitz cvs pserver anonymous t common-lisp.net "/project/movitz/cvsroot")
    (mutest (github "vseloved"))
    (mw-equiv svn "http://svn.foldr.org/~michaelw/mw-equiv/trunk")
    (mw-tiny-clos git "http://www.foldr.org/~michaelw/projects/mw-tiny-clos.git")
    (mycl-util darcs "http://common-lisp.net/project/bdb/darcs/mycl-util")
    ;; XXXX: This user is now missing on github.
    ;; (n3 (github "turbo24prg"))
    (named-readtables darcs "http://common-lisp.net/project/editor-hints/darcs/named-readtables")
    (net-xml-generator (github "franzinc")
     :asd none)
    (netkit darcs "http://fresh.homeunix.net/~luke/misc/repo/netkit"
     :asd none)
    (nfs (github "franzinc")
     :asd none)
    (nibbles (github "froydnj"))
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
    (objcffi (repo-or-cz))
    (ocml (github "kmi"))
    (okra (github "aerique")
     :asd ("okra-bindings.asd"
           "okra-bindings-generator.asd"
           "okra-common.asd"
           "okra-mygui.asd"
           "okra.asd"))
    (ometa hg "http://subvert-the-dominant-paradigm.net/repos/hgwebdir.cgi/ometa")
    ;; XXXX: svn: REPORT of '/svnroot/open-axiom/!svn/vcc/default':
    ;; Could not read response body: SSL error: Decryption has failed.
    ;; (https://open-axiom.svn.sourceforge.net)
    (open-axiom (sourceforge svn)
     :asd none)
    (outbreak (github "patzy"))
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
    ;; XXXX: broken last time I tried to update this repository.
    (parse-js git "http://marijn.haverbeke.nl/git/parse-js")
    (parser-generator git "http://git.nklein.com/lisp/apps/parser-generator.git"
     :asd ("com.nklein.parser-generator.asd"
           "com.nklein.parser-generator.reader.asd"
           "com.nklein.parser-generator.types.asd"))
    ;; (patg (clnet ))  XXXXXXXXXXX Subversion ???
    (patron (github "vy"))
    ;; XXXX: blog entry says persistent-sheeple is being renamed.  code has
    ;; disappeared from github.
    ;; (persistent-sheeple (github "sykopomp"))
    (pg (clnet cvs))
    (pileup (github "nikodemus"))
    (pipes git "git://git.b9.com/pipes.git")
    (pithy-xml (github "frodef"))
    (pjb-lisp darcs "http://darcs.informatimago.com/lisp"
     :asd ("cl-posix/cliki/cliki.asd"
           "cl-posix/cliki/clposixcliki.asd"
           ;; "clext/system.asd"
           ;; "clisp/system.asd"
           ;; "clmisc/system.asd"
           ;; "common-lisp/system.asd"
           ;; "sbcl/system.asd"
           ;; "susv3/system.asd"
           ))
    (pjb-small-cl-pgms darcs "http://darcs.informatimago.com/darcs/public/small-cl-pgms"
     :asd none)
    ;; XXXX: This repository has disappeared on github.
    ;; (plantae (github "patzy"))
    (plexippus-xpath (clnet darcs)
     :asd ("xpath.asd"))
    (plop (google-code svn)
     :asd ("src/plop.asd"))
    (png-read (github "Ramarren"))
    (portableaserve (sourceforge cvs)
     :asd ("acl-compat/acl-compat.asd"
           "aserve/aserve.asd"
           "aserve/htmlgen/htmlgen.asd"
           "aserve/webactions/webactions.asd"
           ;; "libs/cl-ppcre/cl-ppcre-test.asd"
           ;; "libs/cl-ppcre/cl-ppcre.asd"
           ;; "libs/puri-1.3.1/puri.asd"
           ))
    ;; XXXXX What version of postmodern is canonical?  marijn has a version
    ;; XXXXXXXXX
    ;;    (postmodern git "http://marijn.haverbeke.nl/git/postmodern" :asd none)
    ;; there is a copy here too:     http://github.com/marijnh
    (postmodern (clnet darcs)
     :asd ("postmodern.asd"
           "cl-postgres.asd"))
    (postoffice git "git://git.b9.com/postoffice.git")
    (prepl (gitorious))
    (pretty-function (clnet darcs))
    (protobuf (github "brown")
     :asd ("protobuf.asd"
           "protobuf-test.asd"
           "example/protobuf-example.asd"))
    (ptester git "git://git.b9.com/ptester.git")
    (pubmed git "git://git.b9.com/pubmed.git")
    (puri git "git://git.b9.com/puri.git")
    ;; XXXX: I had to permanently accepted a bogus expired certificate.
    (pvs svn "https://spartan.csl.sri.com/svn/public/pvs/trunk"
     :asd none)
    (qbook darcs "http://common-lisp.net/project/bese/repos/qbook")
    (quicklisp-projects (github "quicklisp")
     :asd none)
    (quickproject (github "xach"))
    ;; XXXX: rename to quicktime if possible
    ;; XXXX: try loading it and resolve all dependencies
    (quicktime-ffi (harmon)
     :asd ("quicktime.asd"
           "quicktime-init.asd"
           "quicktime-test.asd"))
    (raylisp (github "nikodemus")
     :asd ("raylisp.asd"
           "raylisp-gui.asd"))
    ;; XXXX: This user is now missing on github.
    ;; (rdf-store (github "turbo24prg"))
    ;; XXXX: This user is now missing on github.
    ;; (rdf-utils (github "turbo24prg"))
    (readline git "git://git.b9.com/cl-readline.git")
    ;; Emacs extensions for lisp.
    (redshank darcs "http://www.foldr.org/~michaelw/projects/redshank"
     :asd none)
    (regenerate-websites (github "gwkkwg"))
    (repo-install (github "jpalmucci"))
    (restas (github "archimag"))
    (restas-planet (github "archimag"))
    (retrospectiff (harmon))
    (reversi git "git://git.b9.com/reversi.git")
    (rfc2109 (clnet darcs))
    (rfc2388 (clnet cvs))
    (rfc2822 (clnet cvs))
    (rjson (github "gonzojive"))
    (rlc git "git://git.b9.com/rlc.git")
    (ropes (github "Ramarren"))
    (rss git "git://git.b9.com/cl-rss.git")
    (rt git "git://git.b9.com/rt.git")
    (rtf darcs "http://common-lisp.net/project/bese/repos/rtf")
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
    (salza2 (xach))
    (sapaclisp (clnet cvs))
    (sb-cga (github "nikodemus"))
    (sb-cpu-affinity (github "nikodemus"))
    (sb-lset (github "nikodemus")
     :asd none)
    ;; XXXX: this repository is missing now
    ;; (sb-mailbox (github "nikodemus")
    ;;  :asd none)
    (sb-vector-io (github "nikodemus"))
    (sbcl (sourceforge cvs)
     :asd none)
    (sbcl-git git "git://git.boinkor.net/sbcl"
     :asd none)
    (screamer (repo-or-cz)
     :asd none)
    (series (sourceforge cvs))
    (sheeple (github "sykopomp"))
    (shuffletron (github "ahefner"))
    ;; XXXX: This repository appears to be the canonical one, but is
    ;; sicl-OLD from "http://dept-info.labri.fr/~strandh/SICL/SICL.git"
    ;; still getting updates?
    (sicl (clnet git "SICL")
     :asd ("Lisp-Unit/lisp-unit.asd"
           "Code/Cons-high/cons-high.asd"
           "Code/Loop/loop.asd"
           "Code/Reader/read.asd"
           "Code/sicl.asd"
           "Code/Format/format.asd"
           "Code/Sequences/sequences.asd"))
    ;; Another sicl project -- a SPARQL implementation.
    ;; (sicl (github "turbo24prg") :asd none)
    (simple-rgb (google-code svn))
    (skippy (xach))
    (slime (clnet cvs)
     :asd ("swank.asd"))
    (slime-proxy (github "3b")
     :asd ("slime-proxy.asd"
           "slime-parenscript.asd"))
    (slitch darcs "http://fresh.homeunix.net/~luke/misc/repo/slitch"
     :asd ("src/netlib.asd"))
    (slurp (github "brown"))
    (smarkup (harmon)
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
    (spatial-trees darcs "http://rvw.doc.gold.ac.uk/sullivan/darcs/spatial-trees")
    (squirl (github "sykopomp"))
    (sst (github "vy"))
    (st-json darcs "http://marijn.haverbeke.nl/st-json")
    (stumpwm git "git://git.savannah.nongnu.org/stumpwm.git")
    (submarine (clnet darcs)
     :asd ("submarine.asd"
           "submarine-tests.asd"))
    (sw-db (gitorious "sw-db-void"))
    (sw-http (gitorious "sw-http-void"))
    (sw-mvc (gitorious "sw-mvc-void"))
    (sw-stm (gitorious "sw-stm-void"))
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
    (symbolicweb (gitorious "symbolicweb-void")
     :asd ("symbolicweb.asd"
           "symbolicweb-examples.asd"
           "symbolicweb-jquery.asd"))
    (t-system (gitorious)
     :asd none)
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
    (tester (github "franzinc"))
    (tiff-ffi (harmon)
     :asd ("tiff-ffi.asd"
           "tiff-ffi-gen.asd"))
    (tilde (github "xach"))
    (time-interval (harmon))
    (tinaa darcs "http://common-lisp.net/project/tinaa"
     :asd ("tinaa.asd"
           "tinaa-test.asd"))
    (trivial-backtrace darcs "http://common-lisp.net/project/trivial-backtrace"
     :asd ("trivial-backtrace.asd"
           "trivial-backtrace-test.asd"))
    (trivial-configuration-parser svn "svn://unmutual.info/trivial-configuration-parser")
    (trivial-features darcs "http://common-lisp.net/~loliveira/darcs/trivial-features"
     :asd ("trivial-features.asd"
           "trivial-features-tests.asd"))
    (trivial-garbage darcs "http://common-lisp.net/~loliveira/darcs/trivial-garbage")
    (trivial-gray-streams cvs pserver anonymous t common-lisp.net "/project/cl-plus-ssl/cvsroot")
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
    (ucw-presentations darcs "http://common-lisp.net/project/ucw/repos/ucw-presentations")
    (ucw_ajax darcs "http://common-lisp.net/project/ucw/repos/ucw_ajax"
     :asd none)
    (ucw_dev darcs "http://common-lisp.net/project/ucw/repos/ucw_dev"
     :asd ("ucw.asd"))
    (uffi git "git://git.b9.com/uffi.git"
     :asd ("uffi.asd"
           "uffi-tests.asd"))
    (umlisp git "git://git.b9.com/umlisp.git"
     :asd ("umlisp.asd"
           "umlisp-tests.asd"))
    (umlisp-orf git "git://git.b9.com/umlisp-orf.git"
     :asd ("umlisp-orf.asd"
           "umlisp-orf-tests.asd"))
    (upstream (github "nikodemus")
     :asd none)
    (uri (github "franzinc")
     :asd none)
    (uri-template (github "vsedach"))
    (url-rewrite darcs "http://common-lisp.net/~loliveira/ediware/url-rewrite")
    (usocket svn "svn://common-lisp.net/project/usocket/svn/usocket/trunk"
     :asd ("usocket.asd"
           "usocket-test.asd"))
    (vclos darcs "http://common-lisp.net/~crhodes/vclos")
    (vcs-tree git "git://git.b9.com/vcs-tree.git")
    (vecto (xach))
    (verrazano darcs "http://common-lisp.net/project/fetter/darcs/verrazano"
     :asd ("verrazano.asd"
           "verrazano-runtime.asd"))
    (vivace-graph (github "kraison"))
    (wclas git "http://cl-www.msi.co.jp/projects/wclas/wclas.git")
    (webfunk (github "gonzojive")
     :asd ("webfunk.asd"
           "contrib/metafunk/metafunk.asd"))
    (weblocks-dev hg "http://bitbucket.org/S11001001/weblocks-dev"
     :asd none)
    (weblocks-stable hg "http://www.bitbucket.org/skypher/weblocks-stable"
     :asd ("contrib/s11001001/weblocks-s11.asd"
           "contrib/yarek/examples/employer-employee/employer-employee.asd"
           "contrib/yarek/examples/weblocks-demo-popover/weblocks-demo-popover.asd"
           "contrib/yarek/weblocks-yarek.asd"
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
    (white-shadow (gitorious "white-shadows"))
    (wiki-parser (github "archimag"))
    (wilbur darcs "http://www.crispylogics.com/opensource/repos/wilbur"
     :asd ("src/wilbur.asd"))
    (wispylisp darcs "http://common-lisp.net/project/wispylisp"
     :asd ("src/wispylisp.asd"))
    (wol git "git://git.b9.com/wol.git")
    (woolly git "http://git.nklein.com/lisp/libs/woolly.git/"
     :asd ("woolly.asd"
           "woolly-gl.asd"))
    (wxcl (sourceforge svn))
    (x.let-star (github "ks" "X.LET-STAR"))
    (x.fdatatypes (github "ks" "X.FDATATYPES"))
    (xarray (github "tpapp"))
    ;; Also available at "http://common-lisp.net/project/xcvb/git/xcvb.git"
    ;; Which is best?
    (xcvb (github "fare")
     :asd ("xcvb.asd"
           "xcvb-driver.asd"
           "xcvb-master.asd"))
    (xe2 (github "dto"))
    (xhtmlgen (github "hanshuebner"))
    (xlunit git "git://git.b9.com/xlunit.git")
    (xml-mop (github "gonzojive"))
    (xmlisp (google-code svn)
     :asd none)
    (xmlutils git "git://git.b9.com/xmlutils.git")
    (xptest git "git://git.b9.com/xptest.git")
    (xuriella (repo-or-cz))
    (yaclml darcs "http://common-lisp.net/project/bese/repos/yaclml")
    (yashmup (github "sykopomp")
     :asd ("yashmup.asd"
           "util/sprite-checker/sprite-checker.asd"))
    (yason (github "hanshuebner"))
    (yotta-zoomer (repo-or-cz))
    (zcdb (github "xach"))
    (zip (clnet cvs))
    (zlib (github "franzinc")
     :asd none)
    (zpb-exif (xach))
    (zpb-ttf (xach))
    (zpng (xach))
    (zs3 (github "xach"))

    ;;; Repositories not related to Common Lisp.

    (dwarf git "http://git.androdna.com/lisp/dwarf" :asd none)
    ;; Emacs is now a bazaar repository that's huge.
    ;; (emacs (savannah bzr)  :lightweight t :asd none)
    (emacs-jabber (sourceforge cvs) :asd none)
    (git git "git://git.kernel.org/pub/scm/git/git.git" :asd none)
    (go (google-code hg) :asd none)
    (libsigsegv (savannah cvs) :asd none)
    (magit git "git://gitorious.org/magit/mainline.git" :asd none)
    (gnome-common git "git://git.gnome.org/gnome-common" :asd none)
    (gnome-terminal git "git://git.gnome.org/gnome-terminal" :asd none)
    (vte git "git://git.gnome.org/vte" :asd none)
    (wave-client-for-emacs (google-code hg) :asd none)
    (wave-protocol (google-code hg) :asd none)
    )
  "Database of projects we are interested in cloning locally.")


;;;     Source code repositories


(defun clnet (project-name scms
              &optional (module (string-downcase project-name)))
  "Repository specification abbreviation function for a project hosted on
common-lisp.net."
  (let ((name (string-downcase project-name)))
    (ecase scms
      ((cvs) `(cvs pserver anonymous t common-lisp.net ,(concat "/project/" name "/cvsroot")
               :module ,module))
      ((darcs) `(darcs ,(concat "http://common-lisp.net/project/" name "/darcs/" module)))
      ((git) `(git ,(concat "git://common-lisp.net/projects/" name "/" module ".git")))
      ((svn) `(svn ,(concat "svn://common-lisp.net/project/" name "/svn/trunk"))))))

(defun melis (project-name)
  "Repository specification abbreviation function for a git project hosted by
Gabor Melis on quotenil.com."
  (let ((repository (string-downcase project-name)))
    `(git ,(concat "http://quotenil.com/git/" repository ".git"))))

(defun github (project-name user
               &optional (repository (string-downcase project-name)))
  "Repository specification abbreviation function for a git project hosted on
github.com."
  `(git ,(concat "git://github.com/" user "/" repository ".git")))

(defun gitorious (project-name
                  &optional (repository (string-downcase project-name)))
  "Repository specification abbreviation function for a git project hosted on
gitorious.org."
  (let ((name (string-downcase project-name)))
    `(git ,(concat "git://gitorious.org/" name "/" repository ".git"))))

(defun google-code (project-name scms
                    &optional (repository (string-downcase project-name)))
  "Repository specification abbreviation function for a Subversion or
Mercurial repository hosted on code.google.com."
  (ecase scms
    ((hg) `(hg ,(concat "http://" repository ".googlecode.com/hg")))
    ((svn) `(svn ,(concat "http://" repository ".googlecode.com/svn/trunk")))))

(defun harmon (project-name)
  "Repository specification abbreviation function for Cyrus Harmon's git
projects hosted on http://git.cyrusharmon.org/cgi-bin/gitweb.cgi"
  (let ((name (string-downcase project-name)))
    `(git ,(concat "git://cyrusharmon.org/pub/git/" name ".git"))))

(defun dwim-hu (project-name)
  "Repository specification abbreviation function for darcs projects hosted
on dwim.hu."
  (let ((name (string-downcase project-name)))
    `(darcs ,(concat "http://dwim.hu/darcs/" name))))

(defun repo-or-cz (project-name)
  "Repository specification abbreviation function for git projects hosted on
repo.or.cz."
  (let ((name (string-downcase project-name)))
    `(git ,(concat "git://repo.or.cz/" name ".git"))))

(defun savannah (project-name scms)
  "Repository specification abbreviation function for a project hosted on
savannah.gnu.org."
  (let ((name (string-downcase project-name)))
    (ecase scms
      ((bzr) `(bzr ,(concat "http://bzr.savannah.gnu.org/r/" name "/trunk")))
      ((cvs) `(cvs pserver anonymous nil cvs.savannah.gnu.org ,(concat "/sources/" name)
               :module ,name)))))

(defun sourceforge (project-name scms)
  "Repository specification abbreviation function for a project hosted on
sourceforge.net."
  (let ((name (string-downcase project-name)))
    (ecase scms
      ((cvs) `(cvs pserver anonymous nil
               ,(intern (string-upcase (concat name ".cvs.sourceforge.net")))
               ,(concat "/cvsroot/" name)
               :module ,name))
      ((git) `(git ,(concat "http://" name ".sourceforge.net/git/" name "/.git")))
      ((svn) `(svn ,(concat "http://" name ".svn.sourceforge.net/svnroot/" name "/trunk"))))))

(defun xach (project-name)
  "Repository specification abbreviation function for Zach Beane's git
projects hosted on http://git.xach.com"
  (let ((name (string-downcase project-name)))
    `(git ,(concat "http://git.xach.com/" name ".git"))))


(defun parse-repository-spec (spec)
  "Parse a repository specification.  The form of the repository entry is
either (NAME SCMS SCMS-ARGS MORE-ARGS) or (NAME (ABBREV-FUNC ABBREV-ARGS)
MORE-ARGS).  In the latter case, ABBREV-FUNC is invoked on (cons NAME
ABBREV-ARGS) to create SCMS and SCMS-ARGS."
  (ecase (type-of (second spec))
    ((symbol) spec)
    ((cons) (destructuring-bind (project-name (scms . arguments) &rest rest)
                spec
              `(,project-name ,@(apply scms (cons project-name arguments)) ,@rest)))))

(defun make-database (repositiory-specs)
  "Create the repository database from a list of repository specifications."
  (mapcar #'parse-repository-spec repositiory-specs))

(defparameter *database* (make-database +repositiory-specs+))

(defun find-project (name)
  "Find the repository specification of the project.  Return two values, the
source code management system (SCMS) used by the project and the project
specification with the SCMS removed."
  (let ((project-spec (assoc name *database*)))
    (when project-spec
      (destructuring-bind (project-name scms &rest rest)
          project-spec
        (values scms `(,project-name ,@rest))))))

(defun operate (project-name operation)
  "Perform an operation on a project."
  (multiple-value-bind (scms arguments)
      (find-project project-name)
    (unless scms
      (error "unknown project"))
      ;; Lack of an asd specification means there's one asd file named after
      ;; the project.
      (unless (member :asd arguments)
        (setf arguments (append arguments '(:asd project-name))))
      (apply scms (cons operation arguments))
      (values)))

(defun checkout (project-name)
  "Check out a project."
  (format t "~%checkout ~a~%" project-name)
  (operate project-name 'checkout))

(defun checkout-all ()
  "Check out all the project repositories in the database."
  (loop for (name . rest) in *database* do (checkout name)))

(defun update (project-name)
  "Update a project."
  (format t "~%update ~a~%" project-name)
  (operate project-name 'update))

(defun update-repositories (repository-specs)
  "Update all the projects listed in REPOSITORY-SPECS."
  (loop for (name . rest) in repository-specs do (update name))
  (values))

(defun update-all ()
  "Update all the project repositories in the database."
  (update-repositories *database*))

(defun update-all-starting-with (name)
  "Update all the project repositories in the database with names greater
than or equal to NAME."
  (update-repositories
   (loop for repository-spec in *database*
         when (string>= (first repository-spec) name)
         collect repository-spec)))

#+sbcl
(defmacro with-cwd (directory &body body)
  (let ((original-directory (sb-posix:getcwd))
        (chdir-worked (gensym "chdir-worked")))
    `(let ((,chdir-worked nil))
       (unwind-protect
            (prog ()
               (sb-posix:chdir ,directory)
               (setf ,chdir-worked t)
               ,@body)
         (when ,chdir-worked
           (sb-posix:chdir ,original-directory))))))

#+sbcl
(defun run (program args output)
  (let ((result
         (sb-ext:process-exit-code
          (sb-ext:run-program program args :output output :search t :wait t))))
    (assert (zerop result))))

(defun link (project-directory asd-file-path)
  (let* ((link-target (concat project-directory "/" asd-file-path))
         (slash (position #\/ asd-file-path :from-end t))
         (asd-file (if slash
                       (subseq asd-file-path (1+ slash))
                       asd-file-path))
         (link (concat *systems-root* "/" asd-file)))
    (assert (probe-file link-target))
    (format t "Create link to ~a~%" link-target)
    (run "rm" `("-f" ,link) *terminal-io*)
    (run "ln" `("-s" ,link-target ,link) *terminal-io*)))

(defun create-asd-links (project-directory project-name asd-files)
  (cond ((eq asd-files 'none))
        ((eq asd-files 'project-name)
         (link project-directory (concat project-name ".asd")))
        ((listp asd-files)
         (dolist (asd-file asd-files)
           (link project-directory asd-file)))))


;;;     Source code management systems


(defun cvs (op name method user password host root
            &key asd (module (string-downcase name)))
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
         (run "cvs" `("-z3" ,cvs-root "co" ,module) *terminal-io*)
         (when (not (string= name module))
           (run "mv" `(,module ,name) *terminal-io*))))
      ((update)
       (with-cwd project-directory
         (run "cvs" `("-z3" ,cvs-root "up" "-A" "-P" "-d") *terminal-io*))))
    (create-asd-links project-directory name asd)))

(defun bzr (op name url &key asd lightweight)
  "Check out or update a bazaar repository."
  (let* ((name (string-downcase name))
         (project-directory (concat *source-root* "/" name)))
    (ecase op
      ((checkout)
       (with-cwd *source-root*
         (run "bzr" `("checkout" "--verbose" ,@(when lightweight '("--lightweight")) ,url ,name)
              *terminal-io*)))
      ((update)
       (with-cwd project-directory
         (run "bzr" '("update" "--verbose") *terminal-io*))))
    (create-asd-links project-directory name asd)))

(defun darcs (op name url &key asd)
  "Check out or update a darcs repository."
  (let* ((name (string-downcase name))
         (project-directory (concat *source-root* "/" name)))
    (ecase op
      ((checkout)
       (with-cwd *source-root*
         (run "darcs" `("get" ,url ,name) *terminal-io*)))
      ((update)
       (with-cwd project-directory
         ;; Sometimes a darcs pull fails because there's a leftover lock
         ;; file from a previous interrupted pull.  Should we try to remove
         ;; the lock file, repository/_darcs/lock?
         (run "darcs" '("pull" "--all" "--verbose") *terminal-io*))))
    (create-asd-links project-directory name asd)))

(defun git (op name url &key asd submodules)
  "Check out or update a Git repository.  When SUBMODULES is non-nil, check
out or update all submodules."
  (let* ((name (string-downcase name))
         (project-directory (concat *source-root* "/" name)))
    (ecase op
      ((checkout)
       (with-cwd *source-root*
         (run "git" `("clone" ,url ,name) *terminal-io*)))
      ((update)
       (with-cwd project-directory
         (run "git" '("pull") *terminal-io*))))
    (when submodules
      (with-cwd project-directory
        (run "git" '("submodule" "init") *terminal-io*)
        (run "git" '("submodule" "update") *terminal-io*)))
    (create-asd-links project-directory name asd)))

(defun hg (op name url &key asd)
  "Check out or update a Mercurial repository."
  (let* ((name (string-downcase name))
         (project-directory (concat *source-root* "/" name)))
    (ecase op
      ((checkout)
       (with-cwd *source-root*
         (run "hg" `("clone" ,url ,name) *terminal-io*)))
      ((update)
       (with-cwd project-directory
         (run "hg" '("update") *terminal-io*))))
    (create-asd-links project-directory name asd)))

(defun svn (op name url &key asd)
  "Check out or update a Subversion repository."
  (let* ((name (string-downcase name))
         (project-directory (concat *source-root* "/" name)))
    (ecase op
      ((checkout)
       (with-cwd *source-root*
         (run "svn" `("checkout" ,url ,name) *terminal-io*)))
      ((update)
       (with-cwd project-directory
         (run "svn" '("update") *terminal-io*))))
    (create-asd-links project-directory name asd)))
