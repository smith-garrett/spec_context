# Trying out van de Cruys (2011) approach to gov-rel-dep association measures

import fileinput
import re
import string
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from collections import defaultdict
import os
import glob
from tqdm import tqdm
from scipy.sparse.linalg import svds

def read_standford(files):
    """Takes a (list of) file(s) and returns a nicely formatted set of
    dependent-governor-dependent triples.
    """
    #with fileinput.input(files=files) as f:
    with fileinput.input(files=glob.glob(files + '*.txt')) as f:
        deps = []
        #for lnct, line in enumerate(f):
        for line in f:
        #    if lnct > 100:
        #        break
            # Getting rid of any numbers
            line = re.sub(' ', '', line)  # removing spaces
            line = re.sub('[,\(\)]+', ' ', line)  # removing formatting punct.
            line = re.sub(' [0-9]+', ' xxnum', line)  # replacing numbers with generic number token
            curr = line.split()
            # Stripping any remaining whitespace & punctuation
            excl = string.punctuation + string.whitespace
            curr = [i.strip(excl) for i in curr]
            # Keeping the distinction between deptype root and node ROOT
            curr = [i.lower() if i not in ['root', 'ROOT'] else i for i in curr]
            if '' in curr:
                continue
            # Making sure we're only dealing with triples, just in case
            if len(curr) == 3:
                # Correct a difference between old and new Univ. Deps.
                if curr[0] == 'dobj':
                    curr[0] = 'obj'
                yield curr


def make_assoc_dict(deps, minct=100, svd=False, outpath=None):
#def make_tc_dict(deps, minct=2, laplace=1.0, positive=True, outpath=None):
#def make_tc_dict(deps, mostcommon=0.8, laplace=1.0, positive=True, outpath=None):
    """PMI. Laplace smooothing not currently implemented.
    """
    ctr = defaultdict(int)  # for keeping joint counts
    wordctr = defaultdict(int)
    ctxctr = defaultdict(int)
    print('Getting counts')
    for triple in tqdm(deps):
        ctr[tuple([triple[0]+'-'+triple[1], triple[2]])] += 1
        ctxctr[triple[0]+'-'+triple[1]] += 1
        wordctr[triple[1]] += 1
        wordctr[triple[2]] += 1

    # Enforcing min. ct.
    ctr = {k: v for k, v in ctr.items() if all(x > minct for x in [wordctr[k[1]], ctxctr[k[0]]])}
    wordctr = {k: v for k, v in wordctr.items() if wordctr[k] > minct}
    ctxctr = {k: v for k, v in ctxctr.items() if ctxctr[k] > minct}
    total = sum(v for v in ctr.values())

    print('\n# total triples: {}'.format(total))
    print('# words: {}'.format(len(wordctr)))
    print('# rel-gov pairs: {}\n'.format(len(ctxctr)))

    print('Converting to PMI')
    for k in tqdm(ctr.keys()):
        ctr[k] /= wordctr[k[1]]
        ctr[k] /= ctxctr[k[0]]
        ctr[k] *= total
        ctr[k] = np.log2(ctr[k])


    print('Converting to pandas')
    ctrpd = pd.Series(ctr).reset_index()
    ctrpd.columns = ['Rel-Gov', 'Dep', 'PMI']
    ctrpd['PMI'] = pd.arrays.SparseArray(ctrpd['PMI'])  # Converting to sparse
    if outpath and not svd:
        print('Writing to compressed .csv')
        ctrpd.to_csv(os.path.join(outpath, 'specPMI_' + str(minct) +'cutoff.csv.gz'),
                                 compression='gzip', na_rep=np.nan)
    elif outpath and svd:
        print('Performing SVD w/ {} dimensions'.format(svd))
        print('NOTE: Converting to (sparse) positive PMI for consistency!')
        ctrpd['PMI'].clip(lower=0.0, inplace=True)  # Making it PPMI in place
        #ctrpd = ctrpd.astype(pd.SparseDtype(np.float32, fill_value=0.0))
        ctrpd = ctrpd.pivot(index='Dep', columns='Rel-Gov', values='PMI', ).fillna(0.0)
        print('Density: {}'.format(ctrpd.sparse.density))
        wvecs, singvals, cvecs = svds(ctrpd.sparse.to_coo(), k=svd)
        print('Making word and context symmetric (Levy et al. 2015)')
        wsym = wvecs * np.sqrt(singvals)
        # Transposing so context vectors are in the rows
        csym = cvecs.T * np.sqrt(singvals)
        wsym = pd.DataFrame(wsym, index=ctrpd.index)
        csym = pd.DataFrame(csym, index=ctrpd.columns)
        print('Writing to compressed .csv')
        wsym.to_csv(os.path.join(outpath, 'WordVecs' + str(minct) + 'SVD' + str(svd) + '.csv.gz'), compression='gzip', na_rep=np.nan)
        csym.to_csv(os.path.join(outpath, 'ContextVecs' + str(minct) + 'SVD' + str(svd) + '.csv.gz'), compression='gzip', na_rep=np.nan)
    return ctrpd

if __name__ == '__main__':
    #files = ['/Users/garrettsmith/Google Drive/UniPotsdam/Research/Features/dependency_embeddings/data/BritNatCorp/BritNatCorp/parsedBNC_Aaae.txt',
    #files = '/Users/garrettsmith/Google Drive/UniPotsdam/Research/Features/dependency_embeddings/data/BritNatCorp/BritNatCorp/parsedBNC_Aaal.txt'#,
    #'/Users/garrettsmith/Google Drive/UniPotsdam/Research/Features/dependency_embeddings/data/BritNatCorp/BritNatCorp/parsedBNC_Baag.txt',
    #'/Users/garrettsmith/Google Drive/UniPotsdam/Research/Features/dependency_embeddings/data/BritNatCorp/BritNatCorp/parsedBNC_Baah.txt']
    files = '/Users/garrettsmith/Google Drive/UniPotsdam/Research/Features/dependency_embeddings/data/BritNatCorp/BritNatCorp/'
    print('Reading in parsed corpus')
    deps = read_standford(files)
    print('Calculating associations')
    # minct 10, 50 lead to memory overload
    tc = make_assoc_dict(deps, minct=50, svd=100, outpath='~/Desktop')
    #print(tc.head())

    #plt.hist(tc.TotalCorrelation)
    #plt.yscale('log')
    #plt.show()
