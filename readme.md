# Include files


```
jscl-chrome-extension/
+-------------------- readme.md    
+-------------------- moren-ide.png    
+-- moren/                             a full set for installation of Chrome extensions    
+------- moren.html                    start page
+------- manifest.json                 manifest
+------- backtab.js                    start script
+------- lizard.png                    icon
+------- static/                            compiled files for Moren IDE
+------- css/                               css resources
+------- man/                               online manual
+------- dist/                              js libraries of third parties
+------- addon/                             Moren IDE addon's
+------- j/                                 Download area
L------- images/                            Assets
```

# Moren concept

IDE Moren created for rapidly programming and prototyping programs on JSCL language (subset of Common Lisp) in yours browser.

>Moren - reduction of the from moraine-morena (geological phenomenon).
The ancient moraines, formed the geological landscape of the contemporary world.
Lisp,  also, as an ancient moraines, has identified the tectonics of the modern programming languages. 
When we see the increasingly popular programming language, we see in him the contours of Lisp tectonics.

IDE contains all the minimum necessary functions.

Other additions may be to developed and debugged in Moren IDE, compiled in JSCL and can be added "on the fly".

It has the property of:

- complete openness
- manageability
- extensibility
- customization capabilities for any user requirement

**Moren IDE is not intended for:**

- industrial and/or conveyor development and production
- Redistribution for the end users 


Intended only for research, rapid development and prototyping lisp programm in the browser environment on the local computers.



Integration is made based on the following components:

- `JSCL` Subset of Common Lisp
- `JQConsole`  Interactivity
- `CodeMirror` Built-in editor
- `Klib`  Interaction with the browser API's and the Document Object Model
- `Moren` Some integration environment


Text editing options:

- line editor `JQConsole`
- `CodeMirror` editor in emacs configuration

Download text files for compilation in an environment:

- Implementation of the load function

Connection compiled in JSCL additions, and other JS libraries, images and css resources is performed on the fly.
Just call lisp functions from console.


The available area of the screen is divided into panels:

- `Banner` for output service IDE messages
- `Control` for the location of controls (buttons, menus)
- `Console` for interactive interaction with the environment 

*see bellow*

```
+-----------------------------------------------+
|               banner                          |
+-----------------------------------------------+
|               control                         |
+-----------------------------------------------+
|                                               |
|               console                         |
|                                               |
+-----------------------------------------------+
```

Positioning of the panels in the browser screen coordinates absolute. 
Panel position, dimensions and colors CSS defined.

- General view [!(https://github.com/vlad-km/jscl-chrome-extension/blob/master/moren-ide.png)]


# Development status

Alpha stage

# Realisation

Chrome extension
Tested on Chrome/55.0.2883.87

# Distribution kit

Distributed as completely ready-to-install Chrome extension. Including the compiled programs from Lisp (JSCL) to JS.

Source lisp code for recompilation may take from:

- JSCL   (https://github.com/jscl-project/jscl.git)
- Klib   (https://github.com/vlad-km/klib.git)
- Moren  (https://github.com/vlad-km/moren.git)


# Installation

For more information, see (https://developer.chrome.com/extensions/getstarted).

**Before beginning the installation, make sure that the permissions specified in the supplied distribution kit, acceptable to you.
See file moren/manifest.json for details about used permissions. If necessary, please to the get the necessary information 
about the security policies at performance the extension from (https://developer.chrome.com/extensions/manifest) **

- Visit chrome://extensions in your browser (or select extensions under the Tools menu open up the Chrome menu)

- Ensure that the Developer mode checkbox is checked.

- Click 'Load unpacked extension' to pop up a file-selection dialog.

- Navigate to the directory in which your place unpacked Moren extension files, and select 'moren' subdirectory.

- If the extension is valid, it'll be loaded up and active right away.
 
- If it's invalid, an error message will be displayed. Correct the error, and try again.


# Get started

After installation hover over the browser action badge to see the new tooltip "Lizard".

Click the "Lizard" and have fun.



