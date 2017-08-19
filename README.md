Outrespace
==========

In the beginning was the void.  
`void`

From the creator emerged the word.  
`hello world`

And lo the word was judged to be good.  
`"compilation succeeded"`

Now was creation organized into statements, and from statements, expressions.  And braces arose therewith, to delimit.  
`{}`

The glory of all was proclaimed.  
`printf "Praise Be!"`

The proclamation resounded, and was improved upon.  
`std::cout << "Praise Be!" << std::endl;`

Discord arose from name clashes and operator overloading.  Thus was born argument-dependent lookup.  This, too, was judged to be good.  
`using namespace std;
cout << "Amen" << endl; `

Entities known as namespaces multiplied over the landscape, constraining scope, enabling concision of naming, and promulgating sensible hierarchies.  And creation endured.  

Now was born the programmer, and set forth upon creation as a child out of the womb.  And the world was harsh.  
`"compilation error"`

Organizational webs entwined.  Description interleaved with content.  Vision was obscured, contentment frustrated, enlightenment denied.  

Until epiphany arrived.  
`emacs`

Thus was the namespace whisperer, that tireless tamer of scopes, bestowed a name.  
`outrespace`

May it endure, and do good things.  Though we are taught that all good things must end.  
`exit(0)`

# Description #
Outerspace is a [^1]collection of utilities to manage c++ namespaces via emacs.  Common operations include viewing the namespaces in a file, navigating between them, highlighting them, and altering them.

# Usage #
outrespace.el needs to be in a running emacs instance's `load-path` variable.
An example use-package invocation is:

`(use-package outrespace  
  :after cc-mode  
  :config  
  (outrespace-mode 1))`

## Keys ##
Outrespace defines its own keymap.  You may want to override the default binding to access it, assuming you are not a masochist.  Readers of this document are users of emacs, so I assume nothing.  For the adventurous, it can look something like this:  

`(use-package outrespace  
  :after cc-mode  
  :config  
  (setq outrespace-prefix-key "\C-cn")  
  (outrespace-mode 1)  
  )`

The following sections describe each function present in the outrespace keymap.  So commands would be invoked by first pressing the prefix key, followed by the listed key combination.

### Navigation ###

- `<alt-p>`

  **outre-goto-namespace-previous**
  
  Navigate forward in the current buffer.
- `<alt-n>`

  **outre-goto-namespace-next**
  
  Navigate backward in the current buffer.
- `j`

  **outre-ivy-jump-to-ns**
  
  Jump to a selected namespace by name.  
  If ivy is loaded, duplicate namespaces can be selected by their position in the buffer.  
  Otherwise, the duplicate namespace names will have unique suffixes (such as <1>) appended.  
- `p`

  **outre-print-enclosing-ns-name**
  
  Print the name of the namespace surrounding point, if any exists.
- `h`

  **outre-highlight-ns-by-name**
  
  Highlight a selected namespace by name.

### Manipulation ###
- `n`

  **outre-outre-wrap-namespace-region**
  
  Wrap the current region with a namespace.
- `c`

  **outre-change-ns-name**
  
  Change the name of a namespace, selected by name.
- `C`

  **outre-change-enclosing-ns-name**
  
  Change the name of the namespace surrounding point, if any exists.
- `d`

  **outre-delete-ns-by-name**
  
  Delete the namespace definition of a namespace, selected by name.
- `D`

  **outre-delete-enclosing-ns**
  
  Delete the namespace definition surrounding point, if any exists.

[^1]: hopefully non-sacrilegious 
