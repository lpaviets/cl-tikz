(in-package #:cl-tikz/tiles)

;;; Wang Tiles

(defvar *rao-jeandel*
  (defwangtiles rao-jeandel
    (:green :red :red :red)
    (:green :blue :red :blue)
    (:green :green :green :red)
    (:blue :red :blue :white)
    (:blue :white :blue :blue)
    (:white :red :white :white)
    (:white :blue :green :red)
    (:red :blue :white :blue)
    (:red :white :red :blue)
    (:red :blue :green :green)
    (:green :red :white :red)))

(defvar *kari-culik*
  (defwangtiles kari-culik
    (:red :green :yellow :yellow)
    (:red :yellow :green :yellow)
    (:yellow :green :green :yellow)
    (:yellow :yellow :red :red)
    (:green :green :red :red)
    (:green :yellow :yellow :red)
    (:blue :red :blue :blue)
    (:blue :yellow :blue :green)
    (:blue :red :purple :yellow)
    (:blue :blue :purple :yellow)
    (:purple :red :purple :blue)
    (:purple :yellow :purple :green)
    (:purple :yellow :blue :yellow)))
