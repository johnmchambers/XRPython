import xml.etree.ElementTree as Etree
import RPython
from copy import copy
import nltk

def getPlay(what):
    ''' Parse the file name, or if it's not a string, assume it's
a previously parsed tree.  (Should of course check that).
    '''
    if isinstance(what, str):
        return Etree.parse(what)
    elif isinstance(what, Etree):
        return what
    else:
        return None #should throw error.
        

class Act(object):
    def __init__(self, obj = None, playTitle = None):
        if obj is None:
            self.title = '<Unspecified>'
        else:
            self.title = obj.findtext('TITLE')
        self.data = obj
        self.playTitle = playTitle

def getActs(play):
    ''' Return a list of the acts in the XML object "play".  Each element of the list is
an object of class "Act" with fields "title" and "data" (the XML element for the act).
    '''
    value = [ ]
    acts = play.findall('.//ACT')
    title = play.findtext('TITLE')
    for el in acts:
        act = Act(el, title)
        value.append(act)
    return value

class Scene(object):
    def __init__(self, actTitle = '<Unspecified>', playTitle = '<Unspecified>', obj = None):
        if obj is None:
            self.title = '<Unspecified>'
            self.actTitle = '<Unspecified>'
            self.playTitle = '<Unspecified>'
        else:
            self.title = obj.findtext('TITLE')
            self.actTitle = actTitle
            self.playTitle = playTitle
        self.data = obj

def getScenes(play):
    ''' Return a list of the scenes in the XML object "play".  Each element of the list is
    an object of class "Scene" with fields "title", "act" and "data"
    (the XML element for the scene).  The argument can alternatively be an Act object to obtain a list
    of scenes from just that act.
    '''
    value = []
    if isinstance(play, Act):
        acts = [ play ]
    else:
        acts = getActs(play)
    for act in acts:
        scenes = act.data.findall('.//SCENE')
        ## epilogue and prologue are like scenes within an act
        scenes = act.data.findall('.//PROLOGUE') + scenes + act.data.findall('.//EPILOGUE')
        actTitle = act.title
        playTitle = act.playTitle
        for scene in scenes:
            obj = Scene(actTitle, playTitle, scene)
            value.append(obj)
    return value

class Speech(object):
    def __init__(self, obj = None, act = '<Unspecified>', scene = '<Unspecified>', playTitle = '<Unspecified>', tokens = True, tokenCase = False):
        '''  A Speech object is normally initialized from within the initialization of a complete play, class Play,
        based on parsing an XML file.  In this case and generally, argument obj will be the XML element containing the speech.
        Text in the speech is usually in the text fields of a number of <LINE> tags, but sometimes also in the  tail field of, e.g.,
        a <STAGEDIR> tag.
        The text is stored as a list of strings for the individual lines.  If argument tokens is True, these lines are also tokenized using nltk.word_tokenizer()
        with the tokens stored in a list of lists, parallel to the lines and assigned to the field "tokens". Unless argument tokenCase
        is True, the text in the tokens is lowercased.
        '''
        self.act = act
        self.scene = scene
        self.playTitle = playTitle
        self.lines = [ ]
        self.tokens = [ ]
        self.speaker = '<Unspecified>'
        if isinstance(obj, Speech): # presumably from a subclass, such as Excerpt; only copy the known attributes
            RPython.inherit(self, obj, FALSE)
        elif not obj is None:
            self.speaker = obj.findtext('SPEAKER')
            lines = obj.findall('.//LINE')
            for line in lines:
                text = line.text
                if not isinstance(text, str):
                    for el in list(line):
                        ## this is how STAGEDIR elements seem to work
                        if isinstance(el.tail, str):
                            text = el.tail
                            break
                if isinstance(text, str):
                    self.lines.append(text)
                    if tokens:
                        if not tokenCase:
                            text = text.lower()
                        text = nltk.word_tokenize(text)
                        self.tokens.append(text)
    def getText(self):
        ''' Returns the text of the speech, as an object that will be
        an R character vector when converted.
        '''
        return RPython.vectorR(self.lines, "character")
    def tokenize(self):
        ''' Returns the set of tokens in the speech as a single
        string, with "$" separating lines.
        '''
        text = ""
        for line in self.tokens:
            if isinstance(line, str):
                text = text + "$" + line
        text = text[1:len(text)] # remove 1st "$"
        return text
    def findText(self, text, tokens = True, ignoreCase = True):
        ''' Searches in the Speech object for occurences of the specified
        text and returns a list of the lines in which that text appears.
        Argument `tokens' controls whether the search is in the tokens
        field, in which case `text' must match a token exactly.  Otherwise
        the search is in the speech text for a matching substring.
        If `ignoreCase' is true, the text argument is converted
        to lower case.  Tokens are always lower-cased and the speech
        text will be converted to lower as needed.
        '''
        value = []
        if ignoreCase:
            text = text.lower()
        if tokens:
            lines = self.tokens
        else:
            lines = self.lines
        for i in range(0, len(lines)):
            thisLine = lines[i]
            if not tokens and ignoreCase:
                thisLine = thisLine.lower()
            if text in thisLine: # matches either a token or a substring
                value.append(i)
        return value

class Excerpt(Speech):
    def __init__(self, obj = Speech(), select = None):
        '''An Excerpt is a Speech object with an additional array `select' array, containing the indices for the lines selected.
        It should be initialized with those two arguments.  Note that an empty array, `[ ]' selects nothing.  To indicate that a selection
        has not been defined (so all lines are active), `select' should be `None'.
        '''
        RPython.inherits(self, obj)
        self.select = select
    def highlight(self, wrap = 2, mark = "**", separator = "......"):
        value = [ ]
        if self.select is None:
            for i in range(len(self.lines)):
                value.append(mark + lines[i] + mark)
            return value
        white = " " * len(mark)
        icur = 0
        for j in range(len(self.select)):
            jstar = self.select[j]
            if j > 0:
                i1 = min(jstar, icur + wrap)
                for i in range(icur, i1): # trailing from previous highlight
                    value.append(white + self.lines[i])
                    icur = i + 1
            i0 = max(icur, jstar - wrap)
            if i0 > icur and j > 0: #print the separator
                value.append(white + separator)
            for i in range(i0,jstar):
                value.append(white + self.lines[i])
            value.append(mark + self.lines[jstar])
            icur = jstar+1
        i1 = min(len(self.lines), icur+wrap)
        for i in range(icur, i1):
            value.append(white + self.lines[i])
        return value
            
def toR_Speech(obj):
    obj = copy(obj)
    obj.lines = RPython.vectorR(obj.lines, "character")
    return RPython.toR_class(obj,  "Speech", "thePlay")

RPython.toR_methods["thePlay.Speech"] = toR_Speech           

def getSpeeches(play, tokens = True, tokenCase = False):
    ''' Return a list of the speeches in the XML object "play".  Each element of the list is
    an object of class "Speech" with fields "title", "act", "scene" and "data"
    (the list of lines of text in the speech).  The argument can alternatively be an Act  or Scene
    object to obtain a list of speeches from just that act or scene.

    The argument "tokens" determines whether the original text or a list of tokens is returned; if it is True,
    the argument "tokenCase" determines whether upper/lower case is distinguished in the tokesn.
    '''
    value = []
    if isinstance(play, Scene):
        scenes = [ play ]
    else:
        scenes = getScenes(play)
    for scene in scenes:
        speeches = scene.data.findall('.//SPEECH')
        sceneAct = scene.actTitle
        sceneTitle = scene.title
        playTitle = scene.playTitle
        for speech in speeches:
            obj = Speech(speech, sceneAct, sceneTitle, playTitle, tokens, tokenCase)
            value.append(obj)
    return value

def getPersonae(play):
    ''' Returns a list of character strings describing the personae in the play.  In the style of
    the XML coding, each string is the name of the person as it will appear in speeches given
    by that character, followed by a description.  <NOT YET>:For personae listed in a group rather than
    separately, this function inserts the group description, in square brackets.
    '''
    value = []
    item = play.find('.//PERSONAE') # assume only one
    if item is None:
        return value
    items = item.findall('.//PERSONA')
    for p in items:
        value.append(p.text)
    return value

## some counter functions
def char_counter(table, speech):
    who = speech.speaker
    this_count = 0
    for line in speech.lines:
        if isinstance(line, str): #can be None, apparently
            this_count += len(line)
    if who in table.keys():
        table[who] = this_count
    else:
        table[who] += this_count
    return True

def speaker_counter(table, speech):
    who = speech.speaker
    if not who in table.keys():
        table[who] = [ ]
    table[who].append(speech.tokens)
    return True

def speechTokens(speeches):
    '''A dictionary whose keys are the names of all speakers with speeches in the list.
    The corresponding element is a list of all the speeches spoken by that speaker, with
    each speech converted into a list of its tokens.
    
    The argument can be from the "speeches" field of a Play object or the result
    of any other computation.
    The argument could also be an Act, Scene or Play:  any object for which getSpeeches()
    returns a list of speeches.
    '''
    return speakersTable(speeches, speaker_counter)

def token_counter(table, speech):
    who = speech.speaker
    if not who in table.keys():
        table[who] = [ ]
    these = [ ]
    for line in speech.tokens:
        these = these + line + "$"
    these.pop() # remove last "$"
    table[who] = table[who] + these + [ "/>" ]
    return True

def tokens(speeches):
    '''A dictionary whose keys are all the names of speakers with speeches in the list.
    The corresponding element is a list of all the tokens spoken by each speaker.
    Special tokens '$' and '/>' are inserted to mark the end of lines and the end of individual
    speeches.
    
    The argument can be from the "speeches" field of a Play object or the result
    of any other computation.
    The argument could also be an Act, Scene or Play:  any object for which getSpeeches()
    returns a list of speeches.
    '''
    return speakersTable(speeches,  token_counter)

def exists_counter(table, speech):
    who = speech.speaker
    table[who] = True
    return True

def speakers(speeches):
    '''A list of all the speakers found in the speeches. The argument
    can be a list of speeches or an object (Play, Act, Scene) for which
    getSpeeches() returns such a list.
    '''
    return speakersTable(speeches, exists_counter).keys()


def speakersTable(speeches, counter):
    '''A dictionary whose keys are all the names of speakers with speeches in the list.
    The counter argument will usually be a function of two arguments.  For each speech
    matching a particular speaker, the function will be called with the first argument
    being the dictionary and the second argument the current speech object.
    Counter functions are expected to accumulate something relevant in the dictionary entries
    whose keys are the speaker fields of the speeches.  The default counter function
    just sets the element of the dictionary to True.
    
    The speeches argument can be from the "speeches" field of a Play object or the result
    of any other computation.
    The argument could also be an Act, Scene or Play:  any object for which getSpeeches()
    returns a list of speeches.
    '''
    if not isinstance(speeches, list):
        speeches = getSpeeches(speeches)
    value = { }
    for speech in speeches:
        counter(value, speech)
    return value

## the stopwords data from nltk

from nltk.corpus import stopwords
stopwords = nltk.corpus.stopwords.words('english')

## add some older stopwords

stopwords = stopwords + [ 'thee', 'thou', 'ye', 'thy', 'thine' ]
punctuation = [ '.', ',', '$', '!', ';', ':', "'", "--", "/>", "?" ]

from collections import Counter
import re

def wordsUsed(tokens, includeCommon = False, includePunctuation = False):
    ''' Given a list of tokens, returns a list of the distinct words included.
    Words are converted to lower case for comparison.
    Common words are excluded by default, being defined as the stopwords set from
    NLTK supplemented with a few common words in Elizabethan English.
    Punctuation is also excluded by default.  Optional second & third arguments in
    the call can override if supplied as True.

    The function can also be called with a dictionary whose elements are token lists,
    as returned by the tokens() function. In this case, it calls iself recursively to in
    effect apply the function to each element, returning a corresponding dictionary.
    '''
    if isinstance(tokens, dict):
        value = { }
        speakers = tokens.keys()
        for who in speakers:
            value[who] = wordsUsed(tokens[who])
        return value
    words = Counter()
    for tk in tokens:
        w = tk.lower()
        w = re.sub("[.]$", "", w) # some tokens have a trailing dot
        if w in words:
            continue
        if w in stopwords and not includeCommon:
            continue
        if w in punctuation and not includePunctuation:
            continue
        words.update([ w ])
    return [ w for w in words ]

def searchSpeeches(text, speeches, tokens = True, ignoreCase = None):
    ''' Given a character string, text, and a list of speeches, returns a parallel list
    each element of which is the list of matching lines in the speech, as returned by the
    findText() method.  The arguments token and ignoreCase are passed to that method.
    If token is True, the match will be against the word tokens constructed for the speech list when
    the corresponding play was intialized.  Otherwise the match is against the text of the lines.
    The default for argument ignoreCase is True if tokens is True else False.
    '''
    if ignoreCase is None:
        ignoreCase = tokens
    value = []
    for speech in speeches:
        value.append(speech.findText(text, tokens, ignoreCase))
    return value

def speechFragments(speeches, matches, before = 3, after = 2, filler = "  ......"):
    ''' Given some a list of speeches and a parallel list of matches to lines within each speech, returns a
    constructed list of speech fragments for any of the speeches that have nonempty matches.  All matched lines will
    be included plus some preceding and following lines as specified by the before=
    and after= arguments.
    The list of matched lines will typically come from a call to searchSpeeches().
    '''
    value = [ ]
    for i in range(len(speeches)):
        sp = speeches[i]
        if not (isinstance(sp, Speech) and isinstance(sp.lines, list)):
            continue
        found = matches[i]
        if len(found) > 0:
            value.append(speechFragment(sp, found, before, after, filler))
    return value

def speechFragment(speech, lines, before = 3, after = 2, filler = "  ...... "):
    n = len(lines)
    N = len(speech.lines)
    if n == 0:
        return None
    value = copy(speech)
    out = [ ]
    pos = 0
    for i in range(n):
        this = lines[i]
        first = this - before + 1
        if first <= pos:
            first = pos
        else:
            out.append(filler)
        fence = this + after
        if fence > N:
            fence = N
        for j in range(first, fence):
            out.append(speech.lines[j])
        pos = fence
    if pos < N - 1:
        out.append(filler)
    value.lines = out
    return value


def allFieldStrings(objects, what):
    ''' Given a list of objects, all of which have a string-valued field specified by what,
    returns a list of the distinct strings found in all the elements of objects.
    '''
    flds = { }
    nbad = False
    for obj in objects:
        thisField = getattr(obj, what)
        if isinstance(thisField, str):
            flds[thisField] = True
        else:
            nbad = True
    if nbad:
        raise ValueError("Some fields were not strings")
    return flds.keys()
    

def speechApply(speech, f):
    value = [ ]
    lines = speech.lines
    tokens = speech.tokens
    if hasattr(speech, 'select'):
        iter = speech.select
    else:
        iter = range(len(speech.lines))
    for i in iter:
        if f(i, lines[i], tokens[i], value):
            break
    return value

def speechListMatch(speeches, f):
    '''Apply the search function `f' to the list of Speech or Excerpt objects.
    The value returned is made from a shallow copy of `speeches`.
    Each  element that has one or more matches is promoted to class "Excerpt",
    with the matched lines in field "select".  Note that for elements that are
    Excerpt objects, the new match is only applied to the selected elements.
    Thus, calling `speechListMatch()` on the value of a previous call defines
    elements that match by both criteria.
    '''
    value = copy.copy(speeches)
    for i in range(len(speeches)):
        matches = speechApply(speeches[i], f)
        if len(matches):
            value[i] = Excerpt(speech, matches)
        elif isinstance(value[i], Excerpt):
            value[i] = Speech(value[i]) # reduce to class Speech
    return value
