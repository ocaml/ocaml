case $XARCH in
i386)
  ./configure
  make world.opt
  sudo make install
  cd testsuite && make all
  git clone git://github.com/ocaml/camlp4
  cd camlp4 && ./configure && make && sudo make install
  git clone git://github.com/ocaml/opam
  cd opam && ./configure && make lib-ext && make && sudo make install
  git config --global user.email "some@name.com"
  git config --global user.name "Some Name"
  opam init -y -a git://github.com/ocaml/opam-repository
  opam install -y oasis
  opam pin add -y utop https://github.com/diml/utop
  ;;
*)
  echo unknown arch
  exit 1
  ;;
esac
