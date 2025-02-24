# Version 0.1

## Changes in 0.1.5

- Refactor the default path for opening/saving projects. It will now save by 
  default in the current directory if the `path =` parameter is not precised.
- Can now import directly from a Spectra object, without having to process 
  beforehand. Still needs to be improved, see issue [#27](https://github.com/rformassspectrometry/RuSirius/issues/27)


## Changes in 0.1.4

- Now supports import for negative polarity. 
  See [#24](https://github.com/rformassspectrometry/RuSirius/issues/24)
- Addition of critical issue in the readme file (line 115). 
  Related to issue [#19](https://github.com/rformassspectrometry/RuSirius/issues/19).
- Fix issue on `show()` method. 


## Changes in 0.1.3

- Now can choose or not to delete features already present when importing new
  ones. Through parameter `deleteExistingFeatures =` in `import()`. Default is
  `TRUE`.
- Addition of functionalities to create, delete and list databases. 
  See [#15](https://github.com/rformassspectrometry/RuSirius/issues/15)
- Can now import feature with set adducts if known.
  See [#14](https://github.com/rformassspectrometry/RuSirius/issues/14)
- Small fixes for `show()` method of `Sirius` object.
  See [#8](https://github.com/rformassspectrometry/RuSirius/issues/8)
- Fix spectral Library matching and now can see a summary of full results using 
  `summary()` and `results()` function and precising
  `result.type = "spectralDbMatch"`.
- Added results() for fragmentation tree. accessible using 
  `result.type = "fragTree"`.
  See [#18](https://github.com/rformassspectrometry/RuSirius/issues/18)
- Added a DockerFile, but cannot seem to start Sirius properly. Work in Progress
  See [#17](https://github.com/rformassspectrometry/RuSirius/issues/17)

## Changes in 0.1.2

- Fix installation of Spectra dev version 
- Addition of code to install devel version of xcms in readme.md

## Changes in 0.1.1

- Addition of package used in the vignettes to DESCRIPITON. 
- Change vignette builder to Rmarkdown. Vignettes now go through the R command 
  checks. 

## Changes in 0.1.0 

- Beginning of RuSirius :sunglasses:
