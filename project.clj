(defproject jat_clojure_middleware "0.1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :repositories [["gradleRepo" "https://repo.gradle.org/gradle/libs-releases"]]
  ;:local-repo "~/.m2/repository"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [gorillalabs/neo4j-clj "1.0.0"]
                 [http-kit "2.3.0"]
                 [compojure "1.6.1"]
                 [hiccup "1.0.5"]
                 [aysylu/loom "1.0.2"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/core.async "0.4.500"]
                 [garden "1.3.9"]

                 ;[org.apache.maven/maven-embedder "3.6.0"]
                 ;[org.slf4j/slf4j-simple "1.7.25"]
                 ;[org.apache.maven/maven-compat "3.6.0"]
                 ;[org.eclipse.aether/aether-connector-basic "1.0.2.v20150114"]
                 ;[org.eclipse.aether/aether-transport-wagon "1.0.2.v20150114"]
                 ;;[org.apache.maven.wagon/wagon-http "2.8"]
                 ;[org.apache.maven.wagon/wagon-provider-api "2.8"]
                 ;[org.gradle/gradle-tooling-api "5.4.1"]
                 ;[de.hhu.jat/analyzer "0.0.1"]
                 ]

  :main ^:skip-aot middleware.core
  :target-path "target/%s"
  ;:profiles {:uberjar {:aot :all}}
  :profiles {:docker {:resource-paths ["docker-data-folder"]}}

  )
