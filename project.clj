(defproject binaryage/xpath-finder "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.597"]
                 [org.clojure/core.async "0.7.559"]
                 [binaryage/chromex "0.8.5"]
                 [binaryage/devtools "0.9.11"]
                 [org.clojars.mmb90/cljs-cache "0.1.4"]
                 [prismatic/dommy "1.1.0"]
                 [com.bhauman/figwheel-main "0.2.3"]
                 [com.bhauman/rebel-readline-cljs "0.1.4"]
                 [environ "1.1.0"]]

  :plugins [[lein-cljsbuild "1.1.7":exclusions [[org.clojure/clojure]]]
            [lein-shell "0.5.0"]
            [lein-environ "1.1.0"]
            [lein-cooper "1.2.2"]]

  :source-paths ["src/background"
                 "src/popup"
                 "src/content_script"
                 "resources"]

  :clean-targets ^{:protect false} ["target"
                                    "resources/unpacked/compiled"
                                    "resources/release/compiled"]

  :profiles {:dev
             {:dependencies [[cider/piggieback "0.4.2"]
                             ;; [com.bhauman/figwheel-main "0.2.3"]
                             ;; [com.bhauman/rebel-readline-cljs "0.1.4"]
                             ]
              :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}}


             :checkouts
                                        ; DON'T FORGET TO UPDATE scripts/ensure-checkouts.sh
             {:cljsbuild {:builds
                          {:background {:source-paths ["checkouts/cljs-devtools/src/lib"
                                                       "checkouts/chromex/src/lib"
                                                       "checkouts/chromex/src/exts"]}
                           :popup      {:source-paths ["checkouts/cljs-devtools/src/lib"
                                                       "checkouts/chromex/src/lib"
                                                       "checkouts/chromex/src/exts"]}}}}
             :checkouts-content-script
                                        ; DON'T FORGET TO UPDATE scripts/ensure-checkouts.sh
             {:cljsbuild {:builds
                          {:content-script {:source-paths ["checkouts/cljs-devtools/src/lib"
                                                           "checkouts/chromex/src/lib"
                                                           "checkouts/chromex/src/exts"]}}}}

             :figwheel
             {:figwheel {:server-port    6888
                         :server-logfile ".figwheel.log"
                         :repl           true}}

             :disable-figwheel-repl
             {:figwheel {:repl false}}

             :cooper
             {:cooper {"content-dev"     ["lein" "content-dev"]
                       "fig-dev-no-repl" ["lein" "fig-dev-no-repl"]
                       "browser"         ["scripts/launch-test-browser.sh"]}}

             :release
             {:env       {:chromex-elide-verbose-logging "true"}
              :cljsbuild {:builds
                          {:background
                           {:source-paths ["src/background"]
                            :compiler     {:output-to     "resources/release/compiled/background.js"
                                           :output-dir    "resources/release/compiled/background"
                                           :asset-path    "compiled/background"
                                           :main          xpath-finder.background
                                           :optimizations :advanced
                                           :elide-asserts true}}
                           :popup
                           {:source-paths ["src/popup"]
                            :compiler     {:output-to     "resources/release/compiled/popup.js"
                                           :output-dir    "resources/release/compiled/popup"
                                           :asset-path    "compiled/popup"
                                           :main          xpath-finder.popup
                                           :optimizations :advanced
                                           :elide-asserts true}}
                           :content-script
                           {:source-paths ["src/content_script"]
                            :compiler     {:output-to     "resources/release/compiled/content-script.js"
                                           :output-dir    "resources/release/compiled/content-script"
                                           :asset-path    "compiled/content-script"
                                           :main          xpath-finder.content-script
                                           :optimizations :advanced
                                           :elide-asserts true}}}}}}

  :aliases {
            "fig-main" ["trampoline" "run" "-m" "figwheel.main" "-bb" "popup" "-bb" "contents" "-b" "background" "-r"]
            "release"         ["with-profile" "+release" "do"
                               ["clean"]
                               ["cljsbuild" "once" "background" "popup" "content-script"]]
            "package"         ["shell" "scripts/package.sh"]})
