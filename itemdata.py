from sys import stdin, stdout
from csv import writer
from lxml.html import html5parser

doc = html5parser.parse(stdin)
ns = {'h': 'http://www.w3.org/1999/xhtml'}
w = writer(stdout)
for item in doc.xpath('//h:div[@class="items-container"]/h:li/h:a', namespaces=ns):
    isaacId = item.getparent().get('data-sid')
    classes = item.xpath('string(h:div/@class)', namespaces=ns)
    iconId = next(c.split('r-itm', 1)[1] for c in classes.split() if c.startswith('r-itm'))
    name = item.xpath('string(h:span/h:p[@class="item-title"]/text())', namespaces=ns)
    description = item.xpath('string(h:span/h:p[@class="pickup"]/text())', namespaces=ns)
    w.writerow([isaacId, name, description, iconId, '500', '0'])
