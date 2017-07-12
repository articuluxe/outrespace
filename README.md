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

## Keys ##
Outrespace defines its own keymap; adventurous uses can customize this.  
`(use-package outrespace
  :after cc-mode
  :config 
  (setq outrespace-prefix-key "\C-cx")
  (outrespace-define-prefix global-map)
  )`

### Navigation ###

### Manipulation ###

[^1]: hopefully non-sacrilegious 
