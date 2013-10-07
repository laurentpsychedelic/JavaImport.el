JavaImport.el
=============

**Welcome to the JavaImport.el package for Emacs!**

***!!THIS SOFTWARE IS IN DEVELOPMENT AND NOT YET FUNCTIONAL!!***

## General presentation

This software aims at providing efficient suggestion mechanism to import classes in the import section
of Java (or Groovy) source file in Emacs. 

For example say you want to use the class ```VeryHandySupportClass``` from the ```i.am.a.very.cool.java.library```
package of a library you want to use. You will probably type directly something like

```java
VeryHandySupportClass myObject = new VeryHandySupportClass();
myObject.doVeryCoolStuff("Wow!");
```         

But when it comes to the task of compiling, without adding 

```java
import i.am.a.very.cool.java.library.VeryHandySupportClass;
```

at the top of your source file you will certainly being told a bunch of insanities from ```javac```, like this:

```shell
javac MyClass.java
MyClass.java:3: error: cannot find symbol
        VeryHandySupportClass myObject = new VeryHandySupportClass();
        ^
  symbol:   class VeryHandySupportClass
  location: class MyClass
MyClass.java:3: error: cannot find symbol
        VeryHandySupportClass myObject = new VeryHandySupportClass();
                                         ^
  symbol:   class VeryHandySupportClass
  location: class MyClass
2 errors
```

It is very annoying to:

1. Check the documentation to figure out that the fully-qualified name for your class is ```i.am.a.very.cool.java.library.VeryHandySupportClass```
2. Going to the top of the file
3. Insert manually 

```java
import i.am.a.very.cool.java.library.VeryHandySupportClass;
```

4. Come back to the point you were coding
5. Remember what you were doing at the first place...

```JavaImport.el``` provides a handy suggestion system, based on the name at cursor point. It scans several sources in order to find
classes matching the object pointed. The sources are for the moment:

1. All the source files in the current project directory tree (Java and Groovy source files)
JavaImport.el scans the contents of the files based on Java/Groovy grammar to extract out the classes defined inside

2. The JAR files present inside the current project directory tree
JavaImport.el get the list of .class files encapsulated in the JAR, and provide a list of matching classes

3. A list of custom classes defined in HTML documentation
The basic use of this source is the JDK classes documentation ("allclasses-noframe.html").
It is usefull to get suggestion from standard JDK API.

The suggestions are displayed as a popup menu in which you can select the correct class to import.
When selected the class is added transparently to the imports, without moving the point, thus without kicking you out
of your current mind flow (you must know that context switching is pretty an expensive task in computing :)).

## Installation

TODO!

## Settings

## Licensing information

This software is provided under the terms of the GNU General Public License.
It is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details, at <http://www.gnu.org/licenses/>.