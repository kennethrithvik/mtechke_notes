# -*- coding: utf-8 -*-
"""
Created on Fri Sep 15 00:19:41 2017
Workshop: IE - Named Entity Recognition
@author: issfz
"""

import nltk
from nltk import word_tokenize, pos_tag, ne_chunk

# ===== POS Tagging and NER using NLTK =====

sent = '''Professor Tan Eng Chye, NUS Deputy President and Provost, and Professor 
Menahem Ben-Sasson, President of HUJ signed the joint degree agreement at NUS, 
in the presence of Ambassador of Israel to Singapore Her Excellency Amira Arnon 
and about 30 invited guests, on July 03, 2013.
'''
# In[1]

# The input for POS tagger needs to be tokenized first.
sent_pos = pos_tag(word_tokenize(sent))
sent_pos

# In[1]
# ===== NER using NLTK =====
# The input for the NE chunker needs to have POS tags.
sent_chunk = ne_chunk(sent_pos)
#print(sent_chunk)

# In[1]
# ===== Now try creating your own named entity and noun phrase chunker ====
# We need to define the tag patterns to capture the target phrases and use 
# RegexParser to chunk the input with those patterns.
# Some minimal tag patterns are given here. 

grammar = r"""
  NE: {<NNP>}      # chunk sequences of proper nouns
  NP:                 
      {<DT><NN>}     
"""

cp = nltk.RegexpParser(grammar)
print(cp.parse(sent_pos))

#------------------------------------------------------------------------
# Exercise: modify the above tag patterns to capture the NEs and NPs in the 
# example sentence. 
#-------------------------------------------------------------------------