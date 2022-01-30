# Clash documentation build configuration file.
#
# This file is execfile()d with the current directory set to its
# containing dir.

import os
import sys

sys.path.insert(0, os.path.abspath('.'))

# -- General configuration ------------------------------------------------

# Sphinx configuration
needs_sphinx = '1.6'

extensions = [
    'recommonmark',
    'sphinx.ext.todo',
    'sphinx.ext.mathjax',
    'sphinx.ext.extlinks',
]

templates_path = ['_templates']
source_encoding = 'utf-8-sig'
master_doc = 'index'

# Project information
project = 'Clash'
copyright = '2017-2019, The Clash Developers'
author = 'The Clash Developers'
version = '1.4.7'
release = version

# Syntax highlighting
highlight_language = 'haskell'
pygments_style = 'tango'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
exclude_patterns = ['_build', 'README.md']

# If true, `todo` and `todoList` produce output, else they produce nothing.
todo_include_todos = True

# -- Options for HTML output ----------------------------------------------

html_title = f"Clash Language {release} Manual"
html_short_title = f"Clash {release} Manual"
html_theme_path = ['.']
html_theme = 'sphinx_rtd_theme'
html_logo = None
#html_static_path = ['images']
html_show_copyright = True

htmlhelp_basename = 'ClashManual'

# -- Options for LaTeX output ---------------------------------------------

latex_engine = 'xelatex'

latex_elements = {
    'papersize': 'a4paper'
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
    ( master_doc, 'clash-docs.tex', 'Clash Documentation',
     author, 'manual'),
]

# If true, show page references after internal links.
latex_show_pagerefs = True

# -- Options for manual page output ---------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    ('clash', 'clash', 'the Clash Compiler', author, 1)
]

# -- Options for Texinfo output -------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
    (master_doc, 'Clash', 'Clash Documentation',
     author, 'Clash', 'One line description of project.',
     'Miscellaneous'),
]

# -- Options for Epub output ----------------------------------------------

# Bibliographic Dublin Core info.
epub_title = project
epub_author = author
epub_publisher = author
epub_copyright = copyright

# A unique identification for the text.
#
# epub_uid = ''

# A list of files that should not be packed into the epub file.
epub_exclude_files = ['search.html']
