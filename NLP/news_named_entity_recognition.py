import spacy
from spacy import displacy
from collections import Counter
import en_core_web_sm

nlp = en_core_web_sm.load()

article = nlp('European authorities fined Google a record $5.1 billion on Wednesday for abusing its power in the mobile phone market and ordered the company to alter its practices')

labels = [x.label_ for x in article.ents]
Counter(labels)

[(x.orth_,x.pos_, x.lemma_) for x in [y 
                                      for y
                                      in nlp(str(article)) 
                                      if not y.is_stop and y.pos_ != 'PUNCT']]
