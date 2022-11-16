(define-module (zaijab packages r-xyz)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix hg-download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system r)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages base)
  #:use-module (gnu packages uglifyjs)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages statistics)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public r-fasttime
  (package
    (name "r-fasttime")
    (version "1.1-0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "fasttime" version))
              (sha256
               (base32
		"00290sxfa6nihahm3s9bslpsmyfm0cpziajmr9mg7grxrzx53ia6"))))
    (properties `((upstream-name . "fasttime")))
    (build-system r-build-system)
    (home-page "http://www.rforge.net/fasttime")
    (synopsis "Fast Utility Function for Time Parsing and Conversion")
    (description
     "Fast functions for timestamp manipulation that avoid system calls and take
shortcuts to facilitate operations on very large data.")
    (license license:gpl2)))

(define-public r-lutz
  (package
  (name "r-lutz")
  (version "0.3.1")
  (source (origin
            (method url-fetch)
            (uri (cran-uri "lutz" version))
            (sha256
             (base32
              "15b8gzwykxyqycjba319jpsbny07j8ny9y4lnrg8mcf488ycz57y"))))
  (properties `((upstream-name . "lutz")))
  (build-system r-build-system)
  (propagated-inputs (list r-lubridate r-rcpp))
  (home-page "https://andyteucher.ca/lutz")
  (synopsis "Look Up Time Zones of Point Coordinates")
  (description
   "Input latitude and longitude values or an sf/sfc POINT object and get back the
time zone in which they exist.  Two methods are implemented.  One is very fast
and uses Rcpp in conjunction with data from the Javascript library
(<https://github.com/darkskyapp/tz-lookup/>).  This method also works outside of
countries borders and in international waters, however speed comes at the cost
of accuracy - near time zone borders away from populated centres there is a
chance that it will return the incorrect time zone.  The other method is slower
but more accurate - it uses the sf package to intersect points with a detailed
map of time zones from here:
<https://github.com/evansiroky/timezone-boundary-builder/>.  The package also
contains several utility functions for helping to understand and visualize time
zones, such as listing of world time zones, including information about daylight
savings times and their offsets from UTC. You can also plot a time zone to
visualize the UTC offset over a year and when daylight savings times are in
effect.")
  (license license:expat)))

(define-public r-ggspatial
(package
  (name "r-ggspatial")
  (version "1.1.6")
  (source (origin
            (method url-fetch)
            (uri (cran-uri "ggspatial" version))
            (sha256
             (base32
              "0biisbbqgp5hfgkpvc88xf8xclq1hr95sxcv1jf819rm45ggmc9l"))))
  (properties `((upstream-name . "ggspatial")))
  (build-system r-build-system)
  (propagated-inputs (list r-abind
                           r-ggplot2
                           r-glue
                           r-rlang
                           r-rosm
                           r-scales
                           r-sf
                           r-tibble
                           r-tidyr))
  (home-page "https://paleolimbot.github.io/ggspatial/")
  (synopsis "Spatial Data Framework for ggplot2")
  (description
   "Spatial data plus the power of the ggplot2 framework means easier mapping when
input data are already in the form of spatial objects.")
  (license license:gpl3)))

(define-public r-rosm
  (package
  (name "r-rosm")
  (version "0.2.6")
  (source (origin
            (method url-fetch)
            (uri (cran-uri "rosm" version))
            (sha256
             (base32
              "11nl3602f3wfccwayjd0a3kqs2lyfb09clccvhvalsp0di7ndbhh"))))
  (properties `((upstream-name . "rosm")))
  (build-system r-build-system)
  (propagated-inputs (list r-abind
                           r-curl
                           r-jpeg
                           r-plyr
                           r-png
                           r-prettymapr
                           r-rgdal
                           r-rjson
                           r-sp))
  (home-page "https://github.com/paleolimbot/rosm")
  (synopsis "Plot Raster Map Tiles from Open Street Map and Other Sources")
  (description
   "Download and plot Open Street Map <https://www.openstreetmap.org/>, Bing Maps
<https://www.bing.com/maps> and other tiled map sources.  Use to create basemaps
quickly and add hillshade to vector-based maps.")
  (license license:gpl2)))

(define-public r-prettymapr
  (package
    (name "r-prettymapr")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "prettymapr" version))
              (sha256
               (base32
		"1fg2j6qx828qrg46clrvln2004nlk22yx7wvhr8vig66nc1lm7bd"))))
    (properties `((upstream-name . "prettymapr")))
    (build-system r-build-system)
    (propagated-inputs (list r-digest r-httr r-plyr r-rjson))
    (home-page "https://github.com/paleolimbot/prettymapr")
    (synopsis "Scale Bar, North Arrow, and Pretty Margins in R")
    (description
     "Automates the process of creating a scale bar and north arrow in any package
that uses base graphics to plot in R. Bounding box tools help find and
manipulate extents.  Finally, there is a function to automate the process of
setting margins, plotting the map, scale bar, and north arrow, and resetting
graphic parameters upon completion.")
    (license license:gpl2)))
