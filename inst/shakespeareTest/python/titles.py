import xml.etree.ElementTree as Etree
def titles(y):
    value = []
    for file in y:
        play = Etree.parse(file)
        title = play.findtext('TITLE')
        value.append(title)
    return value
