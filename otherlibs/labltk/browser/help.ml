let text = "\
\032                         OCamlBrowser Help\n\
\n\
USE\n\
\n\
\032  OCamlBrowser is composed of three tools, the Editor, which allows\n\
\032  one to edit/typecheck/analyse .mli and .ml files, the Viewer, to\n\
\032  walk around compiled modules, and the Shell, to run an OCaml\n\
\032  subshell. You may only have one instance of Editor and Viewer, but\n\
\032  you may use several subshells.\n\
\n\
\032  As with the compiler, you may specify a different path for the\n\
\032  standard library by setting OCAMLLIB. You may also extend the\n\
\032  initial load path (only standard library by default) by using the\n\
\032  -I command line option. The -nolabels, -rectypes and -w options are\n\
\032  also accepted, and inherited by subshells.\n\
\032  The -oldui options selects the old multi-window interface. The\n\
\032  default is now more like Smalltalk's class browser.\n\
\n\
1) Viewer\n\
\n\
\032  This is the first window you get when you start OCamlBrowser.  It\n\
\032  displays a search window, and the list of modules in the load path.\n\
\032  At the top a row of menus.\n\
\n\
\032  File - Open and File - Editor give access to the editor.\n\
\n\
\032  File - Shell opens an OCaml shell.\n\
\n\
\032  View - Show all defs  displays the signature of the currently\n\
\032  selected module.\n\
\n\
\032  View - Search entry  shows/hides the search entry just\n\
\032  below the menu bar.\n\
\n\
\032  Modules - Path editor changes the load path.\n\
\032       Pressing [Add to path] or Insert key adds selected directories\n\
\032       to the load path.\n\
\032       Pressing [Remove from path] or Delete key removes selected\n\
\032       paths from the load path.\n\
\n\
\032  Modules - Reset cache rescans the load path and resets the module\n\
\032  cache. Do it if you recompile some interface, or change the load\n\
\032  path in a conflictual way.\n\
\n\
\032  Modules - Search symbol allows to search a symbol either by its\n\
\032  name, like the bottom line of the viewer, or, more interestingly,\n\
\032  by its type. Exact type searches for a type with exactly the same\n\
\032  information as the pattern (variables match only variables),\n\
\032  included type allows to give only partial information: the actual\n\
\032  type may take more arguments and return more results, and variables\n\
\032  in the pattern match anything. In both cases, argument and tuple\n\
\032  order is irrelevant (*), and unlabeled arguments in the pattern\n\
\032  match any label.\n\
\n\
\032  (*) To avoid combinatorial explosion of the search space, optional\n\
\032  arguments in the actual type are ignored if (1) there are to many\n\
\032  of them, and (2) they do not appear explicitly in the pattern.\n\
\n\
\032  The Search entry just below the menu bar allows one to search for\n\
\032  an identifier in all modules, either by its name (? and * patterns\n\
\032  allowed) or by its type (if there is an arrow in the input). When\n\
\032  search by type is used, it is done in inclusion mode (cf. Modules -\n\
\032  search symbol)\n\
\n\
\032  The Close all button is there to dismiss the windows created\n\
\032  by the Detach button. By double-clicking on it you will quit the\n\
\032  browser.\n\
\n\
\n\
2) Module browsing\n\
\n\
\032  You select a module in the leftmost box by either cliking on it or\n\
\032  pressing return when it is selected. Fast access is available in\n\
\032  all boxes pressing the first few letter of the desired name.\n\
\032  Double-clicking / double-return displays the whole signature for\n\
\032  the module.\n\
\n\
\032  Defined identifiers inside the module are displayed in a box to the\n\
\032  right of the previous one. If you click on one, this will either\n\
\032  display its contents in another box (if this is a sub-module) or\n\
\032  display the signature for this identifier below.\n\
\n\
\032  Signatures are clickable. Double clicking with the left mouse\n\
\032  button on an identifier in a signature brings you to its signature,\n\
\032  inside its module box.\n\
\032  A single click on the right button pops up a menu displaying the\n\
\032  type declaration for the selected identifier. Its title, when\n\
\032  selectable, also brings you to its signature.\n\
\n\
\032  At the bottom, a series of buttons, depending on the context.\n\
\032  * Detach copies the currently displayed signature in a new window,\n\
\032    to keep it.\n\
\032  * Impl and Intf bring you to the implementation or interface of\n\
\032    the currently displayed signature, if it is available.\n\
\n\
\032  C-s opens a text search dialog for the displayed signature.\n\
\n\
3) File editor\n\
\n\
\032  You can edit files with it, but there is no auto-save nor undo at\n\
\032  the moment. Otherwise you can use it as a browser, making\n\
\032  occasional corrections.\n\
\n\
\032  The Edit menu contains commands for jump (C-g), search (C-s), and\n\
\032  sending the current selection to a sub-shell (M-x). For this last\n\
\032  option, you may choose the shell via a dialog.\n\
\n\
\032  Essential function are in the Compiler menu.\n\
\n\
\032  Preferences opens a dialog to set internals of the editor and\n\
\032  type checker.\n\
\n\
\032  Lex (M-l) adds colors according to lexical categories.\n\
\n\
\032  Typecheck (M-t) verifies typing, and memorizes it to let one see an\n\
\032  expression's type by double-clicking on it. This is also valid for\n\
\032  interfaces. If an error occurs, the part of the interface preceding\n\
\032  the error is computed.\n\
\n\
\032  After typechecking, pressing the right button pops up a menu giving\n\
\032  the type of the pointed expression, and eventually allowing to\n\
\032  follow some links.\n\
\n\
\032  Clear errors dismisses type checker error messages and warnings.\n\
\n\
\032  Signature shows the signature of the current file.\n\
\n\
4) Shell\n\
\n\
\032  When you create a shell, a dialog is presented to you, letting you\n\
\032  choose which command you want to run, and the title of the shell\n\
\032  (to choose it in the Editor).\n\
\n\
\032  You may change the default command by setting the OLABL environment\n\
\032  variable.\n\
\n\
\032  The executed subshell is given the current load path.\n\
\032  File: use a source file or load a bytecode file.\n\
\032    You may also import the browser's path into the subprocess.\n\
\032  History: M-p and M-n browse up and down.\n\
\032  Signal: C-c interrupts and you can kill the subprocess.\n\
\n\
BUGS\n\
\n\
* When you quit the editor and some file was modified, a dialogue is\n\
\032 displayed asking wether you want to really quit or not. But 1) if\n\
\032 you quit directly from the viewer, there is no dialogue at all, and\n\
\032 2) if you close from the window manager, the dialogue is displayed,\n\
\032 but you cannot cancel the destruction... Beware.\n\
\n\
* When you run it through xon, the shell hangs at the first error. But\n\
\032 its ok if you start ocamlbrowser from a remote shell...\n\
\n\
TODO\n\
\n\
* Complete cross-references.\n\
\n\
* Power up editor.\n\
\n\
* Add support for the debugger.\n\
\n\
* Make this a real programming environment, both for beginners an\n\
\032 experimented users.\n\
\n\
\n\
Bug reports and comments to <garrigue@kurims.kyoto-u.ac.jp>\n\
";;
