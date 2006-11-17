(defpackage :smaf
  (:use :common-lisp) 
  (:export
   #:annot-paths
   #:char-map-add-x
   #:conf-read-string
   #:dump-sentence-analyses
   #:get-all-annot-paths
   #:get-gmap-unifs
   #:get-smaf-lattice-size
   #:get-saf-l-map
   #:instantiate-edge-l-content
   #:l-edgeType
   #:make-saf
   #:make-saf-edge
   #:make-saf-fv
   #:make-saf-lattice
   #:make-saf-meta
   #:point-to-char-point
   #:read-file-to-string
   #:rename-nodes-by-point-order
   #:reset-conf
   #:run-parse-server
   #:run-fspp-server
   #:saf-edge-content
   #:saf-edge-from
   #:saf-edge-id
   #:saf-edge-l-content
   #:saf-edge-source
   #:saf-edge-target
   #:saf-edge-to
   #:saf-edge-type
   #:saf-fs-feature-value2
   #:saf-fs-partial-tree-2-list-partial-tree
   #:saf-fv-feature
   #:saf-fv-value
   #:saf-id
   #:saf-header
   #:saf-lattice
   #:saf-lattice-edges
   #:saf-lattice-nodes
   #:saf-lattice-start-node
   #:saf-lattice-end-node
   #:saf-meta
   #:saf-meta-addressing
   #:saf-meta-document
   #:sort-edges-by-from
   #:to-xml
   #:xml-to-saf-object
   #:x-span
   #:*char-map-add-offset*
   #:*document*
   #:*ersatz-carg-path*
   #:*gmap*
   #:*morph-rule-map*
   #:*unknown-word-type*
   
   #:id #:source #:target #:from #:to #:l-content #:deps
   )
  (:nicknames :saf))

(pushnew :smaf *features*)