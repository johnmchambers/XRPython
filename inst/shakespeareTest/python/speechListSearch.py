def speechListSearch(speeches, f):
    ''' Apply the search function `f` to the elements of the speech list `speeches'.  Return a shallow copy of the list
    with all elements producing matches promoted to "Excerpt" objects with the corresponding selection.  The elements of the
    list may include such "Excerpt" objects, in which case the search is only applied to the selected lines.  This has the
    effect of "and"ing successive searches.
    '''
    value = copy.copy(speeches)
    for i in range(len(value)):
        matches = speechApply(value[i], f)
        if len(matches):
            value[i] = Excerpt(value[i], matches) # add or replace the select attribute
        elif isinstance(value[i], Excerpt):
            value[i] = Speech(value[i]) # downclass the Excerpt to a Speech
    return value
