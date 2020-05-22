(ns middleware.core
  (:require clojure.pprint)
  (:require [clojure.zip :as z])
  (:require [neo4j-clj.core :as db])
  (:use [compojure.route :only [files not-found]])
  (:use [compojure.core :only [defroutes GET POST DELETE ANY context]])
  (:use org.httpkit.server)
  (:require  [org.httpkit.client :as http])
  (:use ring.middleware.params)
  (:require [clojure.data.json :as json])
  (:require [loom.graph :as graph])
  (:require [loom.alg :as alg])
  (:require [loom.alg-generic :as alg-generic])
  (:require [clojure.core.async :as async])
 ; (:import [de.hhu.jat.main StartApp])
  (:use hiccup.core)
  (:gen-class))

(def docker-data (let [dd (clojure.java.io/resource "docker-data")] (if dd (clojure.edn/read-string (slurp dd)) nil)))

(def neo4j-url (if docker-data (:neo4j-url docker-data) "localhost:7687"))
(def analyzer-url (if docker-data (:analyzer-url docker-data) "localhost:8079"))
(def refactor-url (if docker-data (:refactor-url docker-data) "localhost:8078"))


(defn in? [coll i]
  (some #(= i %) coll))

(def local-db (atom nil))

(defmacro q [query arg]
  `(with-open [session# (db/get-session @local-db)]
     (~query session# ~arg)))

(def state (atom {}))

(defmacro cached [atom key expr]
  `(or (get @~atom ~key)
       ~expr))

(defmacro cached-fn [atom key expr]
  `(fn []
     (or (get @~atom ~key)
         ~expr)))

(defmacro cached-state-fn [key expr]
  `(fn []
     (or (get @state ~key)
         ~expr)))

(defn init-db-connection! []
  (swap! local-db (fn [_] (db/connect (str "bolt://" neo4j-url) "neo4j" "password"))))

(defn init-refactor! []
  (clojure.pprint/pprint "init refactor:")
  (clojure.pprint/pprint @(http/post (str "http://" refactor-url "/refactor/init")
             {:form-params (json/read-str (:body @(http/get (str "http://" analyzer-url "/analyze/gitrepo")))  ) }))  )

(db/defquery class-hierarchy
 "MATCH (n1)-[r]->(n2)
    WHERE n1:Java
      AND (n1:Type OR n1:Package)
      AND NOT n1:Artifact
      AND n2:Java
      AND (n2:Type OR n1:Package)
      AND type(r) IN ['CONTAINS', 'DEPENDS_ON', 'DECLARES',
                      'EXTENDS', 'IMPLEMENTS', 'ANNOTATED_BY']
  RETURN n1,type(r) AS type,n2 LIMIT 500;")

(db/defquery class-hierarchy-simple
 "MATCH (n1)-[r]->(n2)
    WHERE n1:Package
    AND n2:Type
  RETURN n1,type(r),n2;")

(db/defquery class-hierarchy-count
  "MATCH (n1:Package)-[r]->(n2:Type)
  RETURN type(r) AS type, COUNT(*) AS count;")

(db/defquery type-count
  "MATCH (n1)-[r]->(n2)
    WHERE n1:Package
    AND n2:Type
  RETURN COUNT(n2) AS count;")

(db/defquery package-count
  "MATCH (n1:Package)-[r]->(n2:Type)
  RETURN n1, COUNT(*) AS count;")

(db/defquery class-hierarchy-full
  "MATCH (n:Package)-[:CONTAINS]->(m) WITH COLLECT(n) as ns, COLLECT(m) as ms MATCH (q1)-[r]->(q2) WHERE (q1 in ns OR q1 in ms) AND (q2 in ns OR q2 in ms) RETURN q1,type(r) AS t,q2")

(db/defquery just-some-data
  "MATCH p =( (n1:Type)-[e:DEPENDS_ON*1..8]->(n1:Type) ) RETURN p LIMIT 5")

(db/defquery all-packages
  "MATCH (n:Package) RETURN n")

(db/defquery root-artifact
  "MATCH (n:Artifact)<-[:CONTAINS]-(temp_dir:Directory)-[:CONTAINS]-(dummy_project:Project)-[:CREATES]->(:Main) RETURN n")

(db/defquery root-artifact-all-packages
  "MATCH (n:Artifact)<-[:CONTAINS]-(temp_dir:Directory)-[:CONTAINS]-(dummy_project:Project)-[:CREATES]->(:Main) WITH n as root_artifact
MATCH (p:Package)<-[:CONTAINS]-(root_artifact) WITH collect(p) as packages
MATCH (p1:Package)-[c:CONTAINS]->(p2:Package) WHERE p1 in packages AND p2 in packages RETURN p1.fqn as from, ID(p1) as from_id,p2.fqn as to ,ID(p2) as to_id ORDER BY p1.fqn")

(db/defquery root-artifact-all-packages-and-classes
  "MATCH (n:Artifact)<-[:CONTAINS]-(temp_dir:Directory)-[:CONTAINS]-(dummy_project:Project)-[:CREATES]->(:Main) WITH n as root_artifact
MATCH (p:Package)<-[:CONTAINS]-(root_artifact) WITH collect(p) as packages
MATCH (p1:Package)-[c:CONTAINS]->(p2) WHERE p1 in packages and (p2:Package or p2:Type) RETURN p1.fqn as from,p2.fqn as to ORDER BY p1.fqn")


(db/defquery reachable-via-contains-from-root-artifact
  "MATCH (n:Artifact)<-[:CONTAINS]-(temp_dir:Directory)-[:CONTAINS]-(dummy_project:Project)-[:CREATES]->(:Main) WITH n as root_artifact
MATCH (a)<-[:CONTAINS*]-(root_artifact) WITH collect(distinct a) as all_reachable_nodes
MATCH (p1)-[:CONTAINS]->(p2) WHERE p2 in all_reachable_nodes
  AND (p2:Package OR p2:Class OR p2:Archive OR p2:Artifact)
  AND NOT p1:Artifact
RETURN p1.fileName as from, ID(p1) as from_id,
       p2.fileName as to, ID(p2) as to_id
       ORDER BY p1.fileName
UNION
MATCH (a:Artifact)-[:CONTAINS]->(rp:Package)
WHERE NOT (:Package)-[:CONTAINS]->(rp)
RETURN a.fileName as from, ID(a) as from_id,
       rp.fileName as to, ID(rp) as to_id")


(db/defquery root-packages
  "Match (n:Package) WHERE NOT (n)<-[:CONTAINS]-(:Package) RETURN n")

(db/defquery contained-packages
  "MATCH (n:Package) WHERE (n)<-[:CONTAINS]-(:Package {:fqn $origin}) RETURN n")

(db/defquery conflicts
  "MATCH (n1:Type),(n2:Type),(j:Artifact)-[]->(n1)
  WHERE n1.fqn = n2.fqn AND n1 <> n2 AND n1.fqn STARTS WITH \"de\"
  RETURN n1,j")

(db/defquery classpath-conflicts
  "MATCH (n:Class) WITH n.fqn AS fqn, collect(n) AS ns WHERE size(ns) > 1 RETURN fqn")

(db/defquery classpath-conflicts-archives
  "MATCH (dir:Archive)-[:CONTAINS*]->(n:Class)
       WITH n.fqn AS fqn, dir.fileName AS df, collect(n) AS ns WHERE size(ns) > 1
   RETURN DISTINCT df")

(db/defquery classpath-conflicts-root-archives
  "MATCH (dir:Archive)-[:CONTAINS*]->(n:Class)
       WHERE NOT (:Archive)-[:CONTAINS*]->(dir)
           WITH n.fqn AS fqn, dir.fileName AS df, collect(n) AS ns WHERE size(ns) > 1
               RETURN DISTINCT df")

(db/defquery dependency-circles-in-package
  "MATCH (rp:Package {fqn: $packagename})-[:CONTAINS]->(n1:Class),
         p=((n1:Class)-[:DEPENDS_ON*2..3]->(n1:Class))
  RETURN p,rp LIMIT 100")

(db/defquery get-worst-cyclomatic-complexity-methods
  "Match (c:Class)-[:DECLARES]->(m:Method) WHERE m.cyclomaticComplexity > 0
    Return c.fqn, m.name, m.cyclomaticComplexity
    ORDER BY m.cyclomaticComplexity DESC Limit 100")

(db/defquery package-listing
  "MATCH (n1:Type:Java)-[:DEPENDS_ON]->(n2:Type:Java),(package:Package {fqn: $packagename}) WHERE (package)-[:CONTAINS]->(n1) AND (package)-[:CONTAINS]->(n2) RETURN n1,n2,package")

(db/defquery package-listing-fast
"MATCH (p:Package {:fqn $packagename})
WITH p LIMIT 1
MATCH (p)
      -[:CONTAINS]->(n1:Type:Java)
      -[:DEPENDS_ON]->
      (n2:Type:Java)
      <-[:CONTAINS]-(p)
RETURN n1,n2,p AS package")

(db/defquery package-listing-id
  "MATCH (n1:Type:Java)-[:DEPENDS_ON]->(n2:Type:Java),
  (package:Package)
WHERE ID(package) = $nodeID
AND (package)-[:CONTAINS]->(n1)
AND (package)-[:CONTAINS]->(n2)
RETURN n1,n2,package")

(db/defquery package-listing-id-fast
  "MATCH (p:Package) WHERE ID(p) = $nodeID
WITH p LIMIT 1
MATCH (p)
      -[:CONTAINS]->(n1:Type:Java)
      -[:DEPENDS_ON]->
      (n2:Type:Java)
      <-[:CONTAINS]-(p)
RETURN n1,n2,p AS package")

(defn sources [g]
  (map second
       (filter (comp (partial = 0) first)
               (map #(vector (graph/in-degree g %) %) (graph/nodes g)))))
(defn sinks [g]
  (map second
       (filter (comp (partial = 0) first)
               (map #(vector (graph/out-degree g %) %) (graph/nodes g)))))

(defn feedback-arc-set-ordering
  "computes an approximate minimum feedback-arc-set. Outputs a list where all edges from \"right-to-left\" are in the feedback-arc-set.
  This is an implementation after
  Eades, Lin, Smyth,
  A fast and effective heuristic for the feedback arc set problem,
  Information Processing Letters 47 (1993) 319-323."
  [g]
  (letfn [(sources [g]
            (map second
              (filter (comp (partial = 0) first)
                (map #(vector (graph/in-degree g %) %) (graph/nodes g)))))
          (sinks [g]
            (map second
              (filter (comp (partial = 0) first)
                (map #(vector (graph/out-degree g %) %) (graph/nodes g)))))]

    (loop [gr g s1 nil s2 nil]
      (if (empty? (graph/nodes gr))
        (concat s1 s2)
        (let
          [gr-sinks (sinks gr)
           s2 (concat gr-sinks s2) ;prepend sinks to s2
           gr (apply graph/remove-nodes gr gr-sinks)

           gr-sources (sources gr)
           s1 (concat s1 gr-sources) ;but append sources to s1
           gr (apply graph/remove-nodes gr gr-sources)

           delta-max-node-list (take 1 (sort-by
                                         #(- (graph/in-degree gr %) (graph/out-degree gr %))
                                         (graph/nodes gr)))

           s1 (concat s1 delta-max-node-list)
           gr (apply graph/remove-nodes gr delta-max-node-list)]
          (recur gr s1 s2))))))

(defn feedback-arc-set-ordering-grouped
  [g]
  (let [nodes (feedback-arc-set-ordering g)
        edges (graph/edges g)
        fas-edges (vec (reduce (fn [acc [from to]] (if (> (.indexOf nodes from)
                                                           (.indexOf nodes to))
                                                      (conj acc [from to]) acc)) {} edges))]
    {:nodes nodes
     :edges edges
     :fas-edges fas-edges
     :fas-nodes (vec (set (map first fas-edges)))}))

(defn get-package-listing [name]
  (let [edges (map
                  #(vector (:name (:n1 %)) (:name (:n2 %)))
                  (with-open [session (db/get-session @local-db)]
                    (package-listing-fast session {:packagename name})))
          nodes (vec (set (flatten edges)))]
      {:edges edges
       :nodes nodes}))
(defn get-package-listing-by-id [id]
  (let [edges  (map
                #(vector (:name (:n1 %)) (:name (:n2 %)))
                (with-open [session (db/get-session @local-db)]
                      (package-listing-id-fast session {:nodeID id})))
        nodes (vec (set (flatten edges)))]
    {:edges edges
     :nodes nodes}))

(defn find-circles-in-package ^java.util.List [^String packagename]
  (map
    (comp #(map (fn [n] (vector (.asString (.get n "fqn")) (.id n))) %)
          #(.nodes %) #(.asPath %) :p)
    (with-open [session (db/get-session @local-db)]
      (dependency-circles-in-package session {:packagename packagename}))))

(defn- package-listing-minimal-feedbackarc-ordered-impl [lookupfn]
  (let [listing (lookupfn)
        edges (:edges listing)
        arc-set-nodes (feedback-arc-set-ordering (apply graph/digraph (:edges listing)))]
    {:nodes arc-set-nodes
     :edges edges}))

(defn- package-listing-minimal-feedbackarc-ordered-impl-grouped [lookupfn]
  (let [listing (lookupfn)
        edges (:edges listing)
        res (feedback-arc-set-ordering-grouped (apply graph/digraph (:edges listing)))]
    res))

(defn package-listing-minimal-feedbackarc-ordered-by-name [packagename]
  (package-listing-minimal-feedbackarc-ordered-impl-grouped
   #(get-package-listing packagename)))
(defn package-listing-minimal-feedbackarc-ordered-by-id [id]
  (package-listing-minimal-feedbackarc-ordered-impl-grouped
   #(get-package-listing-by-id id)))

(defn package-listing-to-arc-diagram-format [pl]
  (let [nodes (:nodes pl)
        edges (:edges pl)
        fas-edges (:fas-edges pl)
        fas-nodes (set (:fas-nodes pl))
        n-vals (reduce (fn [acc [f t]] (update acc f inc)) (into {} (map #(vector % 1) nodes)) edges)
        ]
    {:links (map (fn [[f t]] {:target t :source f :value 1}) edges)
     :nodes (map (fn [node] {:name node :id node :n (n-vals node) :grp (if (fas-nodes node) 2 1)}) nodes)})
  )

(defn- get-intra-package-depedency-matrix-impl [data-fn]
  (let [{:keys [nodes edges]}  (data-fn)
        idx-map (into {} (map vec (partition 2 (interleave nodes (range)))))
        mat-init (vec (map (fn [_] (vec (map (fn [_] 0) (range (count nodes))))) (range (count nodes))))
        mat (reduce
                  (fn [acc [from to]]
                    (let [from-idx (idx-map from)
                          to-idx (idx-map to)]
                      (update-in acc [from-idx to-idx] inc)))
                  mat-init
                  edges)]
    {:nodes nodes
     :edges edges
     :nameByIndex (clojure.set/map-invert idx-map)
     :indexByName idx-map
     :matrix mat}))

(defn get-intra-package-depedency-matrix-by-name [packagename]
  (get-intra-package-depedency-matrix-impl
   #(package-listing-minimal-feedbackarc-ordered-by-name packagename)))
(defn get-intra-package-depedency-matrix-by-id [id]
  (get-intra-package-depedency-matrix-impl
   #(package-listing-minimal-feedbackarc-ordered-by-id id)))

(defn package-fit-in-percent [edge-list]
  (let [package-fit-per-component 
        (map
         (fn [component]
           (let [grouped (group-by :package component)]
             (float (/ (last (sort (map (fn [[k group]] (count group)) grouped)))
                       (count component)))))
         (alg/scc (apply graph/digraph edge-list)))]
    (/ (reduce + package-fit-per-component) (count package-fit-per-component))))

(defn fqn-to-package [fqn]
  (.substring fqn 0 (.lastIndexOf fqn ".")))
(comment
  (+ 1 2)

  (=
   (get-intra-package-depedency-matrix-by-name
    "de.prob.statespace")

   (get-intra-package-depedency-matrix-by-id
    146)

   )
  (json/read-str (json/write-str (json/read-str "[\"mystr\",1,2,3]")))
  (clojure.pprint/pprint "[[\"0\",\"Shell\"]]")
  (json/read-str "[[\"0\",\"Shell\"]]")

  (edge-bundling-info 866)
  (edge-bundling-info 3262)


  (with-open [session (db/get-session @local-db)]

    (dependencies-below-package session {:nodeid 1030 :packageid 866}))
  (layer-pro-1 866)



  (get-infos-about-node 866)

  )


(db/defquery dependencies-below-package
  "MATCH (n)-[:DEPENDS_ON]->(s)<-[:CONTAINS*..5]-(p) WHERE ID(n)=$nodeid AND ID(p)=$packageid RETURN s.fqn as fqn LIMIT 500")


(db/defquery all-dependencies-below-package
  "MATCH (n)-[:DEPENDS_ON]->(s)<-[:CONTAINS*..5]-(p) WHERE ID(p)=$packageid RETURN n.fqn as from, ID(n) as fromid, s.fqn as to, ID(s) as toid LIMIT 500")

(db/defquery all-java-below-package
  "MATCH (s:Java)<-[:CONTAINS]-(n)<-[:CONTAINS*..5]-(p) WHERE ID(p)=$packageid RETURN n.fqn as from, ID(n) as fromid, s.fqn as to, ID(s) as toid LIMIT 500")

(db/defquery all-packages-below-package
  "MATCH (n:Package)<-[:CONTAINS*..5]-(p) WHERE ID(p)=$packageid RETURN n.fqn as name, ID(n) as id LIMIT 500")

(db/defquery id-to-fqn
  "MATCH (n) WHERE ID(n)=$id RETURN n.fqn as fqn")

(defn package-fit-in-percent [edge-list]
  (let [package-fit-per-component 
        (map
         (fn [component]
           (let [grouped (group-by :package component)]
             (float (/ (last (sort (map (fn [[k group]] (count group)) grouped)))
                       (count component)))))
         (alg/scc (apply graph/digraph edge-list)))]
    (/ (reduce + package-fit-per-component) (inc (count package-fit-per-component))))) ;todo: possible div by zero. inc is only hotfix
;keep close to 1
(defn layer-pro-1 [packageid]
  (with-open [session (db/get-session @local-db)]
    (package-fit-in-percent
     (map (fn [p]
            [{:id (:fromid p) :package (fqn-to-package (:from p))}
             {:id (:toid p) :package (fqn-to-package (:to p))}])
          (all-dependencies-below-package session {:packageid packageid})))))

(defn condensation [edge-list]
  (let [g (apply graph/digraph edge-list)
        scc (alg/scc g)
        component-by-id (into {} (for [node (graph/nodes g)] [node (filter (fn [component] (some #{node} component)) scc)]))]
    (filter (fn [[from to]] (not= from to))
            (apply concat (for [component scc]
                            (map (fn [successor] [component successor])
                                 (vec (distinct (apply concat (for [node component]
                                                                (map
                                                                 (fn [successor] (vec (flatten (get component-by-id successor))))
                                                                 (graph/successors g node))))))))))))
(defn all-pairs-shortest-paths [g]
  (reduce
   (fn [m path] (assoc-in m [(first path) (last path)] path))
   {}
   (filter identity
           (for [v1 (graph/nodes g) v2 (graph/nodes g)]
             (alg/shortest-path g v1 v2)))))

(defn number-of-all-pairs-shortest-paths [g]
  (count
   (filter identity
    (for [v1 (graph/nodes g) v2 (graph/nodes g)]
      (alg/shortest-path g v1 v2)))))

(defn shortest-paths [g]
  (filter identity
          (for [v1 (graph/nodes g) v2 (graph/nodes g)]
            (alg/shortest-path g v1 v2))))

(defn packages-from-connected-component [component]
  (map (fn [node] (:package node)) component))

(defn diff-fn [a b scc-graph contains-graph package-to-id]
  (let [pa (map package-to-id (packages-from-connected-component a))
        pb (map package-to-id (packages-from-connected-component b))
        contains-len (let [paths (for [x pa y pb] (alg/shortest-path contains-graph x y))]
                       (if (some nil? paths)
                         nil
                         (count (max-key count paths))))]
    (if (= contains-len nil)
      nil
      (Math/abs (- (count (alg/shortest-path scc-graph a b))
                   contains-len)))))
;keep close to 0
(defn layer-pro-2 [packageid]
  (with-open [session (db/get-session @local-db)]
    (let [dependency-edges
          (map (fn [p]
                 [{:id (:fromid p) :package (fqn-to-package (:from p))}
                  {:id (:toid p) :package (fqn-to-package (:to p))}])
               (all-dependencies-below-package session {:packageid packageid}))
          contains-edges
          (map (fn [p]
                 [(:fromid p)
                  (:toid p)])
               (all-java-below-package session {:packageid packageid}))

          package-to-id (into {(:fqn (first (id-to-fqn session {:id packageid}))) packageid} (map (fn [v] [(:name v) (:id v)]) (all-packages-below-package session {:packageid packageid})))

          condensed-graph (apply graph/digraph (condensation dependency-edges))
          contains-graph (apply graph/digraph contains-edges) ;AAAAAAAAAAAAAAAAAAAAAAAAAH
          all-shortest-paths (shortest-paths condensed-graph)
          all-diffs (map (fn [path] (diff-fn (first path) (last path) condensed-graph contains-graph package-to-id)) all-shortest-paths)
          ]
      (if (some nil? all-diffs)
        1
        (/ (reduce + all-diffs)
           (count all-diffs))))))
;active when < 70%
(defn layer-contra-1 [packageid]
  (with-open [session (db/get-session @local-db)]
    (let [dependency-edges
          (map (fn [p]
                 [{:id (:fromid p) :package (fqn-to-package (:from p))}
                  {:id (:toid p) :package (fqn-to-package (:to p))}])
               (all-dependencies-below-package session {:packageid packageid}))

          package-to-id (into {(:fqn (first (id-to-fqn session {:id packageid}))) packageid} (map (fn [v] [(:name v) (:id v)]) (all-packages-below-package session {:packageid packageid})))

          condensed-graph (apply graph/digraph (condensation dependency-edges))]
      (/ (count (graph/nodes condensed-graph)) (* 2 (count package-to-id))))))

(defn seperator-set-for-scc [g component]
  (let [component-nodeset (set component)]
    (filter identity
     (for [n component]
       (if (empty? (clojure.set/difference
                    (set (concat (graph/successors g n) (graph/predecessors g n)))
                    component-nodeset))
         nil
         n)))))
;is active when large. When to small, there is no contra argument. (keep above 80% to be active)
(defn layer-contra-2 [packageid]
  (with-open [session (db/get-session @local-db)]
    (let [dependency-edges
          (map (fn [p]
                 [{:id (:fromid p) :package (fqn-to-package (:from p))}
                  {:id (:toid p) :package (fqn-to-package (:to p))}])
               (all-dependencies-below-package session {:packageid packageid}))
          package-to-id (into {(:fqn (first (id-to-fqn session {:id packageid}))) packageid} (map (fn [v] [(:name v) (:id v)]) (all-packages-below-package session {:packageid packageid})))
          condensed-graph (apply graph/digraph (condensation dependency-edges))
          sccs (alg/scc (apply graph/digraph dependency-edges))
          seperator-sets-size (map (fn [component] (/ (count (seperator-set-for-scc (apply graph/digraph dependency-edges) component)) (count component))) sccs)]
      (/ (reduce + seperator-sets-size) (count seperator-sets-size)))))
;active=below 10%
(defn ports-and-adapters-pro-1 [packageid]
  (with-open [session (db/get-session @local-db)]
    (let [dependency-edges
          (map (fn [p]
                 [{:id (:fromid p) :package (fqn-to-package (:from p))}
                  {:id (:toid p) :package (fqn-to-package (:to p))}])
               (all-dependencies-below-package session {:packageid packageid}))
          package-to-id (into {(:fqn (first (id-to-fqn session {:id packageid}))) packageid} (map (fn [v] [(:name v) (:id v)]) (all-packages-below-package session {:packageid packageid})))
          sccs (alg/scc (apply graph/digraph dependency-edges))
          sccs-packages (map (fn [component] (if (< (count component) 4) 0 (/ (count (set (map :package component))) (count component)))) sccs)]
      (/ (reduce + sccs-packages) (count sccs-packages)))))
;active=below 5%
(defn ports-and-adapters-pro-2 [packageid]
  (with-open [session (db/get-session @local-db)]
    (let [dependency-edges
          (map (fn [p]
                 [{:id (:fromid p) :package (fqn-to-package (:from p))}
                  {:id (:toid p) :package (fqn-to-package (:to p))}])
               (all-dependencies-below-package session {:packageid packageid}))
          package-to-id (into {(:fqn (first (id-to-fqn session {:id packageid}))) packageid} (map (fn [v] [(:name v) (:id v)]) (all-packages-below-package session {:packageid packageid})))
          condensed-graph (apply graph/digraph (condensation dependency-edges))
          sccs (alg/scc (apply graph/digraph dependency-edges))]
      (if (< (count (graph/nodes condensed-graph)) 4)
        0
        (/ (count (sinks condensed-graph)) (count (graph/nodes condensed-graph)))))))
;active when below 20%
(defn ports-and-adapters-contra-1 [packageid]
  (with-open [session (db/get-session @local-db)]
    (let [dependency-edges
          (map (fn [p]
                 [{:id (:fromid p) :package (fqn-to-package (:from p))}
                  {:id (:toid p) :package (fqn-to-package (:to p))}])
               (all-dependencies-below-package session {:packageid packageid}))
          package-to-id (into {(:fqn (first (id-to-fqn session {:id packageid}))) packageid} (map (fn [v] [(:name v) (:id v)]) (all-packages-below-package session {:packageid packageid})))
          condensed-graph (apply graph/digraph (condensation dependency-edges))
          sccs (alg/scc (apply graph/digraph dependency-edges))]
      (/ (count sccs) (count dependency-edges)))))

(db/defquery jqa-node-info
  "MATCH (n)
WHERE ID(n) = $nodeID
RETURN n")
(defn factory-pro-1 [nodeid]
  (with-open [session (db/get-session @local-db)]
    (let [name (get-in (first (jqa-node-info session {:nodeID nodeid})) [:n :name])]
      (.endsWith (.toLowerCase name) "factory"))))
(db/defquery jqa-declared-methods
  "MATCH (n)-[:DECLARES]->(m:Method)
WHERE ID(n) = $nodeID
RETURN m")
(defn factory-pro-2 [nodeid]
  (with-open [session (db/get-session @local-db)]
    (let [classname (get-in (first (jqa-node-info session {:nodeID nodeid})) [:n :name])
          return-types (set (filter #(not= "void" %) (map (comp first #(clojure.string/split % #" ") :signature :m) (jqa-declared-methods session {:nodeID nodeid}))))]
      (if (and (= (count return-types) 1)
               (not= (first return-types) classname))
        true
        false))))

(defn factory-contra-1 [nodeid]
  (with-open [session (db/get-session @local-db)]
    (let [return-types (set (map (comp first #(clojure.string/split % #" ") :signature :m) (jqa-declared-methods session {:nodeID nodeid})))]
      (if (and (= (count return-types) 1)
               (= (first return-types) "void"))
        true
        false))))

(defn singleton-pro-1 [nodeid]
  (with-open [session (db/get-session @local-db)]
    (let [name (get-in (first (jqa-node-info session {:nodeID nodeid})) [:n :name])]
      (.endsWith (.toLowerCase name) "singleton"))))

(db/defquery jqa-declared-constructors
  "MATCH (n)-[:DECLARES]->(m:Constructor)
WHERE ID(n) = $nodeID
RETURN m")
(defn singleton-pro-2 [nodeid]
  (with-open [session (db/get-session @local-db)]
    (let [return-types (set (map (comp :visibility :m) (jqa-declared-constructors session {:nodeID nodeid})))]
      (if (and (= (count return-types) 1)
               (= (first return-types) "private"))
        true
        false))))
(db/defquery jqa-declared-fields
  "MATCH (n)-[:DECLARES]->(m:Field)
WHERE ID(n) = $nodeID
RETURN m")
(defn singleton-pro-3 [nodeid]
  (with-open [session (db/get-session @local-db)]
    (let [classname (get-in (first (jqa-node-info session {:nodeID nodeid})) [:n :name])
          fields (map (comp :m) (jqa-declared-fields session {:nodeID nodeid}))
          static-fields (filter #(:static %) fields)]
      (not (empty? (filter (fn [sf] (= classname (first (clojure.string/split (:signature sf) #" ")))) static-fields))))))


(defn builder-pro-1 [nodeid]
  (with-open [session (db/get-session @local-db)]
    (let [name (get-in (first (jqa-node-info session {:nodeID nodeid})) [:n :name])]
      (.endsWith (.toLowerCase name) "builder"))))

(defn builder-pro-2 [nodeid]
  (with-open [session (db/get-session @local-db)]
    (let [classname (get-in (first (jqa-node-info session {:nodeID nodeid})) [:n :name])
          non-static-methods (filter #(not (:static %)) (map :m (jqa-declared-methods session {:nodeID nodeid})))
          methods-returning-this-class (filter (fn [sf] (= classname (first (clojure.string/split (:signature sf) #" ")))) non-static-methods)]
      (> 0.7 (/ (count methods-returning-this-class) (count non-static-methods))))))

(defn builder-pro-3 [nodeid]
  (with-open [session (db/get-session @local-db)]
    (let [classname (get-in (first (jqa-node-info session {:nodeID nodeid})) [:n :name])
          non-static-methods (filter #(not (:static %)) (map :m (jqa-declared-methods session {:nodeID nodeid})))
          methods-returning-this-class (filter (fn [sf] (= classname (first (clojure.string/split (:signature sf) #" ")))) non-static-methods)]
      (= 1 (- (count non-static-methods) (count methods-returning-this-class))))))

(comment
  (layer-pro-1 10915)

  (layer-pro-2 10915)

  (layer-contra-1 10915)

  (layer-contra-2 10915)

  (ports-and-adapters-pro-1 10915)

  (ports-and-adapters-pro-2 10915)

  (float (ports-and-adapters-contra-1 10915))



  (first (first '(([[{:id 3450, :package "de.prob.statespace"}]
              [{:id 9293, :package "de.prob.model.eventb"}]]
             [[{:id 3450, :package "de.prob.statespace"}]
              [{:id 9295, :package "de.prob.model.eventb"}]]))))


  (alg/all-pairs-shortest-paths (graph/digraph [:a :b] [:b :c] [:c :d] [:c :g] [:d :e] [:f :e] [:g :f] [:g :h]))

  (all-pairs-shortest-paths (graph/digraph [:a :b] [:b :c] [:c :d] [:c :g] [:d :e] [:f :e] [:g :f] [:g :h]))
  (shortest-paths (graph/digraph [:a :b] [:b :c] [:c :d] [:c :g] [:d :e] [:f :e] [:g :f] [:g :h]))

  (alg/shortest-path (graph/digraph [:a :b] [:b :c] [:c :d] [:c :g] [:d :e] [:f :e] [:g :f] [:g :h]) :a :g)

  (condensation [[:a :b] [:b :c] [:c :a] [:c :d] [:d :e] [:e :f] [:f :d] [:f :g] [:g :h]])

  (factory-pro-1 7808)

  (factory-pro-1 7808)
  (builder-pro-2 4584)
  (builder-pro-3 4584)

  )

(defn edge-bundling-info [packageid]
  (let [relevant-classes (with-open [session (db/get-session @local-db)]
                           (db/execute
                            session
                            (str "MATCH (my_package) WHERE ID(my_package) = " packageid " WITH my_package "
                                 "MATCH (my_package)-[:CONTAINS*..4]->(n:Type) "
                                 "RETURN n.fqn as fqn, ID(n) as id LIMIT 150")))]
    (with-open [session (db/get-session @local-db)]
      (doall
       (map (fn [c] (assoc c
                           :imports
                           (vec (map :fqn (dependencies-below-package session {:nodeid (:id c) :packageid packageid})))
                           :name (:fqn c)
                           :size (:id c)
                           ))
            relevant-classes)))))

(defn edge-bundling-handler [req]
  (let [nodeid (Long/parseLong ((:params req) "id"))]
    (edge-bundling-info nodeid)))

(defn node-search [req]
  (let [json-body (json/read-str (slurp (clojure.java.io/reader (:body req) :encoding "UTF-8")))]
    (clojure.pprint/pprint "hello")
    (clojure.pprint/pprint json-body)

    (with-open [session (db/get-session @local-db)]
      (db/execute
       session
       (str "MATCH (n) WHERE "
            (clojure.string/join
             " AND "
             (map
              (fn [[t ex]]
                (cond
                  (= t "0") (str "\"" ex "\" = n.name")
                  (= t "1") (str "\"" ex "\" in labels(n)")
                  (= t "2") (str "\"" ex "\" = n.visibility")
                  :default (str "n." t " = \"" ex "\"")
                  ))
              json-body
              )
             )
            " RETURN n.name as name, labels(n) as labels, n.fqn as fqn, ID(n) as id"
            )))))

(defn sources-of-class-hierarchy [edges]
  (let [source-vertices (set (pmap :q1 edges))
        sink-vertices (set (pmap :q2 edges))
        graph-sources (clojure.set/difference source-vertices sink-vertices)]
    graph-sources))


(defn sources-from-edge-list [edge-list]
  (clojure.set/difference (set (map first edge-list))
                          (set (map second edge-list))))

(defn sinks-from-edge-list [edge-list]
  (clojure.set/difference (set (map second edge-list))
                          (set (map first edge-list))))

(defn to-d3tree
  ([edge-list]
   (let [roots (sources-from-edge-list edge-list)
         root (some identity roots)
         root-edges (filter #(= (first %) root) edge-list)]
     (if (> (count roots) 1)
       {:ERROR :THERE_WAS_MORE_THAN_ONE_ROOT!}
       {:name root
        :children (vec (map (partial to-d3tree edge-list) (map second root-edges)))})))
  ([edge-list root]
   (let [root-edges (filter #(= (first %) root) edge-list)]
     (if (empty? root-edges)
       {:name root}
       {:name root
        :children (vec (map (partial to-d3tree edge-list) (map second root-edges)))})
     )))
(defn build-full-path [edge-list edge]
  (let [prev-edge (first (filter #(= (second %) (first edge)) edge-list))]
    (if prev-edge
      (recur edge-list (cons (first prev-edge) edge))
      edge)))
(defn build-all-full-paths [edge-list]
  (pmap (partial build-full-path edge-list) edge-list))
(defn calc-walk-path-in-tree [in-tree path]
  (conj
   (vec (interleave
     (repeat :children)
     (loop [tree in-tree
            graph-path path
            path-so-far []]
       (if (<= (count graph-path) 1)
         path-so-far
         (let [index (first (first
                             (filter #(= (second graph-path) (:name (second %)))
                                     (map-indexed vector (:children tree)))))]
           (recur
            (nth (:children tree) index)
            (rest graph-path)
            (conj path-so-far index)))))
     ))
   :children))

(defn add-edge-to-tree [tree [from to]]
  (update-in tree (calc-walk-path-in-tree tree from)
             #(vec (conj % {:name to}))))
(defn add-path-to-tree [tree path]
  (update-in tree (calc-walk-path-in-tree tree (drop-last path))
             #(vec (conj % {:name (last path)}))))
(defn build-d3tree-from-edge-list [edge-list]
  (reduce add-path-to-tree
          {:name (some identity (sources-from-edge-list edge-list))} ;root node
          (sort-by count (build-all-full-paths edge-list))))

(defn package-hierarchy-from-root []
  (let [query-res (with-open [session (db/get-session @local-db)]
                    (root-artifact-all-packages session))
        res (vec (map #(vector (:from %) (:to %)) query-res))
        idmap (into {} (concat
                        (map #(vector (:from %) (:from_id %)) query-res)
                        (map #(vector (:to %) (:to_id %)) query-res)))]
    {:idmap idmap
     :tree (build-d3tree-from-edge-list res)}))
(defn package-hierarchy-from-root-with-classes []
  (let [query-res (with-open [session (db/get-session @local-db)]
                    (root-artifact-all-packages-and-classes session))
        res (vec (map #(vector (:from %) (:to %)) query-res))
        idmap (into {} (concat
                        (map #(vector (:from %) (:from_id %)) query-res)
                        (map #(vector (:to %) (:to_id %)) query-res)))]
    {:idmap idmap
     :tree (build-d3tree-from-edge-list res)}))

(defn package-hierarchy-from-root-with-classes-complete []
  (let [query-res (with-open [session (db/get-session @local-db)]
                   (reachable-via-contains-from-root-artifact session))
       res (vec (map #(vector (str (:from %)
                                   " (" (:from_id %) ")")
                              (str (:to %)
                                   " (" (:to_id %) ")"))
                     query-res))
        idmap (into {} (concat
                        (map #(vector (:from %) (:from_id %)) query-res)
                        (map #(vector (:to %) (:to_id %)) query-res)))
       ]
    {:idmap idmap
     :tree (build-d3tree-from-edge-list res)}))



(defn progress-report [req]
  (-> @state :progress-report))

(defn neo4j-server-ready? []
  (= "success"
     (get (last (json/read-str (:body @(http/get (str "http://" analyzer-url "/analyze/status")))))
          "status")))

(defn accumulate [{:keys [root-hierarchy-complete]}]
  (let []))

(db/defquery cyclo-complex
"MATCH (c)-[:DECLARES]->(m:Method)
WHERE ID(c) in $ids
RETURN ID(c) as id,sum(m.cyclomaticComplexity) as cycloComplexSum")
(db/defquery labels
"MATCH (c)
WHERE ID(c) in $ids
RETURN ID(c) as id,labels(c) as labels")
(defn labels-accum-fn [idmap]
  (let [id2labels (reduce
                   (fn [acc m] (assoc acc (:id m) (set (:labels m))))
                   {}
                   (q labels {:ids (vals idmap)}))]
    (fn [e]
      (if (and id2labels (map? e))
       (assoc e :labels (id2labels (:id e)))
       e))))
(defn cyclo-complex-accum-fn [idmap]
  (let [id2cyclo (reduce
                  (fn [acc m] (assoc acc (:id m) (:cycloComplexSum m)))
                  {}
                  (q cyclo-complex {:ids (vals idmap)}))]
    (fn [e]
      (if (map? e)
        (let [id-of-e (:id e)]
          (if (id2cyclo id-of-e)
            (assoc e :cycloComplexSum (id2cyclo id-of-e))
            (assoc e :cycloComplexSum (reduce +
                                              (map :cycloComplexSum
                                                   (:children e))))))
        e))))
(defn size-accum-fn []
  (fn [e]
    (if (map? e)
      (assoc e :size
             (reduce (fn [acc child] (+ acc (:size child))) 1 (:children e)))
      e)))
(defn id-fn []
  (fn [e]
    (if (map? e)
     (let [id-str (subs (:name e)
                        (inc (clojure.string/index-of (:name e) "("))
                        (dec (.length (:name e))))
           id-of-e (Integer/parseInt id-str)]
       (assoc e :id id-of-e)) e)))

(defn extract-per-node-info-into-transient-fn!
  [transient-map-var]
  (fn [e]
    (if (map? e)
      (do
        (var-set transient-map-var (assoc! @transient-map-var (:id e) (dissoc e :children)))))
    e))

(defn inject-parent-id-fn []
  (fn [e]
    (if (and (map? e) (:children e))
      (update e :children (partial
                           map (fn [ee]
                             (assoc ee :parent
                                    (dissoc e :children)))))
      e)))

(defn add-infos-to-tree [{:keys [tree idmap]}]
  (with-local-vars [id2node (transient (hash-map))]
    {:tree
     (let [restree (clojure.walk/postwalk
                    (comp
                     (extract-per-node-info-into-transient-fn! id2node)
                     (inject-parent-id-fn))
                    (clojure.walk/postwalk
                     (comp (cyclo-complex-accum-fn idmap)
                           (size-accum-fn)
                           (labels-accum-fn idmap)
                           (id-fn))
                     tree))]
       (assoc restree :parent (dissoc restree :children)))
     :idmap idmap
     :id2node (persistent! @id2node)}))

(defn analyze! []
  (swap! state assoc
    :root-hierarchy-complete
    (add-infos-to-tree
     (package-hierarchy-from-root-with-classes-complete))))
(defn jqa-kickoff! [params]
  @(http/post (str "http://" analyzer-url "/analyze/start")
              {:form-params params})
  (async/thread
    (while (not (neo4j-server-ready?))
      (Thread/sleep 100))
    (init-db-connection!)
    (analyze!)
    (init-refactor!)))



(db/defquery just-one-node
"MATCH (n)
WHERE ID(n) = $nodeID
RETURN n,labels(n) as labels")
(defn get-infos-about-node [id]
  (with-open [session (db/get-session @local-db)]
   (let [node-sep (first (just-one-node session {:nodeID id}))
         node-info (assoc (:n node-sep) :labels (:labels node-sep))
         package? (in? (:labels node-info) "Package")
         type? (in? (:labels node-info) "Type")
         dep-matrix (if package?
                      (get-intra-package-depedency-matrix-by-id id)
                      nil)]
     (assoc node-info
            :declared-methods (map :m (jqa-declared-methods session {:nodeID id}))
            :dep-matrix dep-matrix))))

(defn get-infos-about-tree-node [id]
  (let [general-info (get-infos-about-node id)
        node-info ((:id2node (:root-hierarchy-complete @state)) id)
        package? (in? (:labels node-info) "Package")
        type? (in? (:labels node-info) "Type")]
    (merge general-info node-info)))

;--------------------------------------------------------------------------------
(defmacro json-response [handle-fn]
  `(fn [req#]
     {:status 200
      :headers {"Content-Type" "text/json"}
      :body (json/write-str (~handle-fn req#))}))

(defmacro json-response-no-arg [handle-fn]
  `(fn [req#]
     {:status 200
      :headers {"Content-Type" "text/json"}
      :body (json/write-str (~handle-fn))}))

(defn app [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "hello HTTP!"})

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn show-landing-page [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (html [:span {:class "foo"} "bar"])})

(defn web-socket-example [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (html [:span {:class "foo"} "bar"])})

(defn get-user-by-id [req]
  (let [id (-> req :params :id)]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (json/write-str {:a :b
                            :c id
                            :d [12 id
                                {:foo :bar
                                 id id}]})}))

(defn update-user-by-id [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (html [:span {:class "foo"} "bar"])})
(defn async-handler [req]
  {:status 200
     :headers {"Content-Type" "text/html"}
     :body (html [:span {:class "foo"} "bar"])})

(def predefined-projects
  {1 {:path "/home/wiredaemon/test"
      :display "ProB 2 Kernel Test Environment"}
   2 {:path "/I/DO/NOT/EXIST"
      :display "DUMMY ENTRY JUST FOR SHOW"}})

(defn analyze-handler [req]
  (if (not (:started? @state))
    (do
      (swap! state assoc :started? true)
      (let [islocal (= "true" ((:form-params req) "isLocal"))
            path (if islocal
                   (:path
                    (predefined-projects (Integer/parseInt ((:form-params req) "analyzePath"))))
                   ((:form-params req) "analyzePath"))]
        (jqa-kickoff! (:form-params req)))
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (html [:span "Starting Analysis"])})
    {:status 500
     :headers {"Content-Type" "text/html"}
     :body (html [:span "Already Started"])}))

(defn reset-handler [req]
  (reset! state {})
  @(http/post (str "http://" analyzer-url "/analyze/reset"))
  {:status 200 :headers
   {"Content-Type" "text/html"}
   :body (html [:span "reset"])})

(defroutes all-routes
  (GET "/" [] show-landing-page)
  (GET "/api" []
       (fn [req]
         {:status 200 :headers
          {"Content-Type" "text/html"}
          :body (html [:span "jojojo api here"])}))

  (GET "/api/krasserShit" []
       (fn [req]
         {:status 200 :headers
          {"Content-Type" "text/html"}
          :body (html [:span "AM SACK"])}))
  (GET "/api/edgebundling" []
       (json-response edge-bundling-handler))
  (GET "/ws" [] web-socket-example)
  (GET "/async" [] async-handler) ;; asynchronous(long polling)
  ;(GET "/package/:id" [] package-listing-json)
  (POST "/api/analyze" [] analyze-handler)
  (POST "/api/reset" [] reset-handler)
  (context "/api/user/:id" [id]
    (GET "/" [] get-user-by-id)
    (POST "/" [] update-user-by-id))
  (GET "/api/layer-pro-1/:id" [id]
       (json-response-no-arg #(< 0.95 (layer-pro-1
                                (Integer/parseInt id)))))
  (GET "/api/layer-pro-2/:id" [id]
       (json-response-no-arg #(> 0.1 (layer-pro-2
                                       (Integer/parseInt id)))))
  (GET "/api/layer-contra-1/:id" [id]
       (json-response-no-arg #(< 0.7 (layer-contra-1
                                      (Integer/parseInt id)))))
  (GET "/api/layer-contra-2/:id" [id]
       (json-response-no-arg #(< 0.8 (layer-contra-2
                                      (Integer/parseInt id)))))
  (GET "/api/ports-and-adapters-pro-1/:id" [id]
       (json-response-no-arg #(> 0.1 (ports-and-adapters-pro-1
                                      (Integer/parseInt id)))))
  (GET "/api/ports-and-adapters-pro-2/:id" [id]
       (json-response-no-arg #(> 0.05 (ports-and-adapters-pro-2
                                      (Integer/parseInt id)))))
  (GET "/api/ports-and-adapters-contra-1/:id" [id]
       (json-response-no-arg #(> 0.2 (ports-and-adapters-contra-1
                                       (Integer/parseInt id)))))

  (GET "/api/factory-pro-1/:id" [id]
       (json-response-no-arg #(factory-pro-1 (Integer/parseInt id))))
  (GET "/api/factory-pro-2/:id" [id]
       (json-response-no-arg #(factory-pro-2 (Integer/parseInt id))))
  (GET "/api/factory-contra-1/:id" [id]
       (json-response-no-arg #(factory-contra-1 (Integer/parseInt id))))

  (GET "/api/singleton-pro-1/:id" [id]
       (json-response-no-arg #(singleton-pro-1 (Integer/parseInt id))))
  (GET "/api/singleton-pro-2/:id" [id]
       (json-response-no-arg #(singleton-pro-2 (Integer/parseInt id))))
  (GET "/api/singleton-pro-3/:id" [id]
       (json-response-no-arg #(singleton-pro-3 (Integer/parseInt id))))

  (GET "/api/builder-pro-1/:id" [id]
       (json-response-no-arg #(builder-pro-1 (Integer/parseInt id))))
  (GET "/api/builder-pro-2/:id" [id]
       (json-response-no-arg #(builder-pro-2 (Integer/parseInt id))))
  (GET "/api/builder-pro-3/:id" [id]
       (json-response-no-arg #(builder-pro-3 (Integer/parseInt id))))




  (GET "/api/progress" [] (json-response progress-report))
  (GET "/api/total-types" [] (json-response-no-arg
                              #(with-open [session (db/get-session @local-db)]
                                 (first (type-count session)))))
  (GET "/api/total-nodes" [] (json-response-no-arg
                              #(with-open [session (db/get-session @local-db)]
                                 (first (db/execute session "MATCH (n) RETURN count(n) as count")))))
  (GET "/api/total-artifacts" [] (json-response-no-arg
                              #(with-open [session (db/get-session @local-db)]
                                 (first (db/execute session "MATCH (n:Artifact) RETURN count(n) as count")))))
  (GET "/api/total-packages" [] (json-response-no-arg
                                  #(with-open [session (db/get-session @local-db)]
                                     (first (db/execute session "MATCH (n:Package) RETURN count(n) as count")))))
  (GET "/api/package-hierarchy-root-complete" []
       (json-response-no-arg
        (cached-state-fn
         :root-hierarchy-complete
         (package-hierarchy-from-root-with-classes-complete))))
  (GET "/api/package-hierarchy-root" []
       (json-response-no-arg package-hierarchy-from-root-with-classes))
  (GET "/api/package-listing-by-name/:name" [name]
       (json-response-no-arg #(package-listing-to-arc-diagram-format
                               (package-listing-minimal-feedbackarc-ordered-by-name name))))
  (GET "/api/package-listing-by-id/:id" [id]
       (json-response-no-arg #(package-listing-to-arc-diagram-format
                               (package-listing-minimal-feedbackarc-ordered-by-id
                                (Integer/parseInt id)))))
  (GET "/api/node-info/:id" [id]
       (json-response-no-arg #(get-infos-about-node
                               (Integer/parseInt id))))
  (GET "/api/tree-node-info/:id" [id]
       (json-response-no-arg #(get-infos-about-tree-node
                               (Integer/parseInt id))))
  (POST "/api/search" []
        (json-response node-search))
  (files "/static/") ;; static file url prefix /static, in `public` folder
  (not-found "<p>Page not found.</p>")) ;; all other, return 404

;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------

(defn -main
  "Example usage of neo4j-clj"
  [& args]

  (reset! server (run-server (wrap-params #'all-routes) {:port 8081}))

  )

(if (not docker-data)
  (-main))
