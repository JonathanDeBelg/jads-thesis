import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from collections import Counter
from nltk.stem import PorterStemmer
from nltk.stem import WordNetLemmatizer
import re
import pandas as pd
import emoji
from langdetect import detect, LangDetectException

# Ensure necessary NLTK data is downloaded
nltk.download('stopwords')
nltk.download('punkt')
nltk.download('wordnet')

# Language stop words mapping
lang_map = {
    'en': set(stopwords.words('english')),
    'de': set(stopwords.words('german')),
    'es': set(stopwords.words('spanish')),
    'nl': set(stopwords.words('dutch')),
    'it': set(stopwords.words('italian')),
    'sv': set(stopwords.words('swedish')),
    'fi': set(stopwords.words('finnish')),
    'ca': set(stopwords.words('catalan')),
    'pt': set(stopwords.words('portuguese')),
    'id': set(stopwords.words('indonesian')),
    'fr': set(stopwords.words('french')),
    'no': set(stopwords.words('norwegian')),
    'ro': set(stopwords.words('romanian')),
    'hu': set(stopwords.words('hungarian')),
    'da': set(stopwords.words('danish')),
    'tr': set(stopwords.words('turkish')),
}

# Initialize stemmer and lemmatizer
stemmer = PorterStemmer()
lemmatizer = WordNetLemmatizer()

def deemojize(caption):
    caption = emoji.demojize(caption)
    caption = caption.encode('ascii', 'ignore').decode('ascii')
    return caption


def detect_language(text):
    try:
        return detect(text)
    except LangDetectException:
        return 'unknown'

def custom_tokenize(text):
    tokenizer = nltk.tokenize.RegexpTokenizer(r'\#\w+|\w+')
    return tokenizer.tokenize(text)

def preprocess_texts(data, text_column, lang_column):
    def preprocess_text(row):
        text = row[text_column]
        lang = row[lang_column]

        # Get the stop words for the given language
        stop_words = lang_map.get(lang, lang_map['en'])  # Default to English if no mapping is found

        # Remove numbers
        # text = re.sub(r'\d+', '', text)

        # Tokenize the text
        word_tokens = custom_tokenize(text)

        # Remove stop words
        filtered_text = [word for word in word_tokens if word not in stop_words]    

        # Lemmatize the words
        lemmatized_text = [lemmatizer.lemmatize(word) for word in filtered_text if word[0] != '@' and word[0] != '#']

        # Stem the words
        stemmed_text = [stemmer.stem(word) for word in lemmatized_text if word[0] != '@' and word[0] != '#']

        return ' '.join(stemmed_text)

    # Apply the preprocessing function
    return data.apply(preprocess_text, axis=1)