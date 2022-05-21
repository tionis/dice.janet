(declare-project
  :name "dice"
  :description "personal dice rolling library and program"
  :dependencies []
  :author "tionis.dev"
  :license "MIT"
  :url "https://tasadar.net/tionis/dice.janet"
  :repo "https://tasadar.net/tionis/dice.janet")

(declare-source
  :source ["dice.janet"])

#(declare-native
# :name "mynative"
# :source ["mynative.c" "mysupport.c"]
# :embedded ["extra-functions.janet"])

(declare-executable
  :name "dice"
  :entry "dice.janet"
  :install true)
