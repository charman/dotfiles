#!/usr/bin/env python

import argparse
import tarfile
import zipfile


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('tgz_file')
    parser.add_argument('zip_file')
    args = parser.parse_args()

    tarf = tarfile.open(name=args.tgz_file, mode='r|gz')
    zipf = zipfile.ZipFile(file=args.zip_file, mode='w', compression=zipfile.ZIP_DEFLATED)
    for m in tarf:
        if m.isfile():
            f = tarf.extractfile(m)
            fl = f.read()
            fn = m.name
            print(fn)
            zipf.writestr(fn, fl)
    tarf.close()
    zipf.close()


if __name__ == "__main__":
    main()
