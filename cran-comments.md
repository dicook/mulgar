## Overview

* This is a new release.
* Changes made at request of manual check by Beni Altmann: reduced size by removing some sample data sets, fixed return value for four functions

## ── R CMD check results ────────────────────────── mulgar 1.0.0 ────
Duration: 18.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Test environment

* local R installation: R version 4.3.1 (2023-06-16)
* Windows Server 2022, R-devel, 64 bit
* Fedora Linux, R-devel, clang, gfortran
* Ubuntu Linux 20.04.1 LTS, R-release, GCC

using `check_rhub()`, returns some notes:

❯ checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
    
which I understand can be ignored.    
