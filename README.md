# FusionRings
__AUTHORS:__ Gert Vercleyen, Joost Slingerland

This package is designed to explore and calculate properties of fusion rings in a user friendly manner. It contains an extensive (though not exhaustive) list fusion rings: `FusionRingList` (or `FRL`), and many functions for working with fusion rings. The package contains a lot of symbolic expressions which might be simplified much further and you may do so at own risk. Note that functions such as Simplify do not necessarily preserve numerical value due the multivaluedness of complex powers.

This package is released under the MIT license (see LICENSE file in top directory) and for any professional use such as in academia the authors kindly ask to cite the related article on arxiv: (ARXIV CODE)
## Installation instructions
Installing the package from the FusionRings.zip file can be done using steps:

1. Use the command
git clone https://github.com/gert-vercleyen/FusionRings
in a terminal and zip the directory, or download the zip file from https://github.com/gert-vercleyen/FusionRings (by clicking on the green "Code" button right above the list of files and choosing "Download ZIP")
2. Make sure the zip file is called FusionRings.zip. If not, rename it.
2. In Mathematica go to File -> Install... in the top menu
3. For Type of Item to Install click on Application
4. For Source click on From File ... and select the FusionRings.zip file
5. Choose whether you want to install this package for all users or only for yourself and finish the installation by clicking on OK

The package can now be loaded by evaluating
```
<<FusionRings`
```
during a Mathematica session.

If you would like to have access to the characters of fusion rings you should additionally use the following command
```
<<FusionRings`CharacterData`
```
If you would like to have access to modular data of fusion rings you should additionally use the following command
```
<<FusionRings`ModularData`
```
The the modular data and characters are independent from each other and don't interfere if you load both sub-packages.

_Note: the first time you load any of the (sub-)packages will take longer because the packages create data files that are optimized for import._

## Usage instructions
The package folder contains a notebook called FusionRings.nb with information on how to use the package together with a lot of examples.

If you have never used Mathematica before you might want to review the basics from https://www.wolfram.com/language/fast-introduction-for-programmers/en/. After all, looking up data is nice but actually working with data is much more interesting.
