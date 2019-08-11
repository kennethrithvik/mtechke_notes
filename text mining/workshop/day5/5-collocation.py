# -*- coding: utf-8 -*-
"""
Created on Fri Sep 28 23:58:14 2018

@author: issfz
"""

import string
from nltk.collocations import BigramCollocationFinder, TrigramCollocationFinder
from nltk.metrics import BigramAssocMeasures, TrigramAssocMeasures
from nltk.corpus import stopwords, reuters

# Let's use the category \"crude\" documents from reuters corpus
# Use the tokenized words and change them to lowercase
crude_tok = [ reuters.words(f) for f in reuters.fileids('crude') ]
words = [ w.lower() for f in crude_tok for w in f ]

# Find bigram collocations (two-word phrases) from the data
# Get the top 20 collocations using the selected metrics
bcf = BigramCollocationFinder.from_words(words)
top20 = bcf.nbest(BigramAssocMeasures.likelihood_ratio, 10)
top20

# In the above results, although we get useful collocations like \"crude oil\", 
# we also get a lot of noises. Those can be filtered off.
# Let's filter off stopwords and anything less than two characters long.
stopset = set(stopwords.words('english'))
filter_stops = lambda w: len(w) < 3 or w in stopset or w.isdigit()
bcf.apply_word_filter(filter_stops)

# Are the results better now?
bcf.nbest(BigramAssocMeasures.likelihood_ratio, 10)

# There are quite a number of metrics available. Experiment to see which one gives you better collocations.
bcf.nbest(BigramAssocMeasures.chi_sq, 10)
bcf.nbest(BigramAssocMeasures.pmi, 10)
bcf.nbest(BigramAssocMeasures.raw_freq, 10)

# Now try getting the trigram collocations
tcf = TrigramCollocationFinder.from_words(words)
tcf.apply_word_filter(filter_stops)
tcf.nbest(TrigramAssocMeasures.likelihood_ratio, 20)


# Now try finding collocations from documents in another category. Do you get very different phrases?
money_tok = [ reuters.words(f) for f in reuters.fileids('money-fx') ]
words = [ w.lower() for f in money_tok for w in f ]
bcf = BigramCollocationFinder.from_words(words)
bcf.apply_word_filter(filter_stops)
bcf.nbest(BigramAssocMeasures.likelihood_ratio, 20)

# The functions can also be used to find collocations that are not side by side.
bcf2 = BigramCollocationFinder.from_words(words, window_size = 10)
bcf2.apply_word_filter(filter_stops)
bcf2.apply_freq_filter(2)
bcf2.nbest(BigramAssocMeasures.likelihood_ratio, 20)
