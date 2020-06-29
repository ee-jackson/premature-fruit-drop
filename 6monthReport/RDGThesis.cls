% rdgthesis, based on munthesis v1.0
% Memorial University, Newfoundland, Canada
% By C. Dean Barnes <deanb@mun.ca> and Chris Duggan <c.duggan@mun.ca>
% Version 1.0 released 2010/27/07
% Version 1.1 released 2011/03/17
% Version 1.2 released 2011/05/05
%-------------------------- identification ---------------------
%\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{RGDThesis}[2010/27/07 v1.0 Based on MUN LaTex Thesis class]
%-------------------------- Declaration of options -----------------------
%This class uses a standard report class
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}
\ProcessOptions\relax \LoadClass{report}
%-----------------------Package loading---------------------------------
%use expanded unicode
\usepackage[utf8x]{inputenc}
%permit multilingual text output
%\usepackage[T2A, TS1, T1]{fontenc}
%multilingual packages loaded here
\usepackage[greek,english]{babel}
%\usepackage{cjhebrew} 
%\usepackage[encapsulated]{CJK}

%enhanced support of Computer Modern fonts
\usepackage{lmodern}
%additional American Mathematical Society mathematical typesetting capabilities
\usepackage{amsmath}
%American Mathematical Society mathematical theorems
\usepackage{amsthm}
%American Mathematical Society mathematical symbols
\usepackage{amssymb}
%linespacing
\usepackage{setspace}
%permit landscape format
\usepackage{pdflscape}
%%-------------------page setup-----------------------------------------
%\usepackage[left=3.8cm, right=2.5cm, top=3cm, bottom=3cm]{geometry}
%
%%---------------------define command in the title page--------------------
%\newcommand{\submittext}{A (Thesis, Dissertation, or Report) submitted to the
%School of Graduate Studies in partial fulfillment of the requirements for the degree of}
\newcommand{\submittext}{}
\newcommand{\thesisnote}{by}
\newcommand{\ndegree}{}
\newcommand{\faculty}{}


%\newcommand{\school}{Memorial University of Newfoundland}
%\newcommand{\location}{St. John's \hfill Newfoundland}
\newcommand{\school}{School of Biological Sciences}
\newcommand{\location}{University of Reading}
\newcommand{\Abstract}{Abstract}
%
%
% DECLARATIONS
%
% These macros are used to declare arguments needed for the
% construction of the title page and other elements in the preamble.
%

%
%name of the graduate academic unit (e.g. Computer Science, Mathematics and Statistics, Physics and Physical Oceanography)
\def\ndeg#1{\gdef\@ndeg{#1}}
%
\def\fac#1{\gdef\@fac{#1}}

%copyright including name of candiate and year of graduation
\def\copyrightyear#1{\gdef\@copyrightyear{#1}}


%define title page layout
%
\newcommand{\rdgtitlepage}{%
\thispagestyle{empty}%
\begin{center}
%\includegraphics[width=0.4\textwidth]{rdglogo.eps} % needed for latex
\includegraphics[width=0.4\textwidth]{rdglogo.png} % needed for pdflatex
\vskip 2em
\normalfont\normalsize\normalfont\school\\
\vskip 0.5em
\normalfont\Large\bfseries\@title\\
\vskip 0.5em
\normalfont\normalsize\thesisnote\\
\vskip 0.5em

% \normalfont\normalsize\copyright \@author
\normalfont\normalsize \@author
\vskip 1em

\normalfont\normalsize\normalfont\submittext\\
\vskip 2em
\normalfont\bfseries\ndegree \@ndeg
\vskip 0.5em

\normalfont\bfseries\faculty\@fac
\vskip 20em

\normalfont\normalsize\bfseries\@date\\
\vskip 0.5em
\normalfont\normalsize\normalfont\location\\
\vskip 1em
\end{center}}



%-----------end of Title page--------------------

