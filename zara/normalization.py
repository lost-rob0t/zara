"""
Text normalization - clean up transcribed text before sending to Prolog
"""

import re
from typing import List


# Contractions and slang expansions
EXPANSIONS = {
    "can't": "cannot",
    "won't": "will not",
    "don't": "do not",
    "i'm": "i am",
    "you're": "you are",
    "it's": "it is",
    "that's": "that is",
    "what's": "what is",
    "gonna": "going to",
    "wanna": "want to",
    "gimme": "give me",
    "lemme": "let me",
}

# Filler words to remove
FILLERS = {
    "um", "uh", "umm", "uhh", "like", "you know", 
    "i mean", "sort of", "kind of"
}


def normalize_text(text: str) -> str:
    """
    Normalize transcribed text for Prolog
    - lowercase
    - expand contractions
    - remove fillers
    - clean punctuation
    """
    # Lowercase
    text = text.lower().strip()
    
    # Expand contractions
    for contraction, expansion in EXPANSIONS.items():
        text = text.replace(contraction, expansion)
    
    # Remove punctuation except apostrophes
    text = re.sub(r"[^\w\s']", " ", text)
    
    # Tokenize
    words = text.split()
    
    # Remove fillers
    words = [w for w in words if w not in FILLERS]
    
    # Rejoin
    return " ".join(words)


def tokenize(text: str) -> List[str]:
    """Split normalized text into tokens"""
    return normalize_text(text).split()


if __name__ == "__main__":
    # Test
    tests = [
        "Um, like, can't you open Chrome for me?",
        "Set a timer for, uh, 5 minutes",
        "What's the weather gonna be tomorrow?",
    ]
    
    for test in tests:
        print(f"Input:  {test}")
        print(f"Output: {normalize_text(test)}")
        print()
