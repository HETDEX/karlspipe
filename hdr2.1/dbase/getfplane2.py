import numpy as np

try:
    # Python 3
    from urllib.request import urlopen
    from urllib.error import HTTPError
except ImportError:
    # Python 2
    from urllib2 import urlopen, HTTPError


def get_fplane(filename, datestr='', actpos=False, full=True):

    url = 'https://luna.mpe.mpg.de/fplane/' + datestr

    if actpos:
        url += '?actual_pos=1'
    else:
        url += '?actual_pos=0'

    if full:
        url += '&full_fplane=1'
    else:
        url += '&full_fplane=0'

    try:
        resp = urlopen(url)
    except HTTPError as e:
        raise Exception(' Failed to retrieve fplane file, server '
                        'responded with %d %s' % (e.getcode(), e.reason))

    with open(filename, 'w') as f:
        f.write(resp.read().decode())

date = np.loadtxt('datelist')
for i in range(365):
    d0=int(date[i])
    d1=str(date[i])
    d2="fp"+str(d0)
    print(d0)
    get_fplane(d2,d1,True,False)
