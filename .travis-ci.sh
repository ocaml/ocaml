case $XARCH in
i386)
  uname -a
  ./configure
  make world.opt
  sudo make install
  cd testsuite && make all
  git clone git://github.com/ocaml/camlp4
  cd camlp4 && ./configure && make && sudo make install
  git clone -b 1.1 git://github.com/ocaml/opam
  cd opam && ./configure && make && sudo make install
  opam init -y -a
  opam install utop
  ;;
*)
  echo unknown arch
  exit 1
  ;;
esac
