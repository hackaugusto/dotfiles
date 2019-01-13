#!/usr/bin/python
import requests


def omdb_query(**params):
    response = requests.get(
        f'http://www.omdbapi.com/',
        params=params,
    )
    assert response.status_code == 200
    return response.json()


def omdb_format(response):
    r = response
    return (
        f"{r['Title']} - {r['Year']} ({r['imdbRating']})\n"
        f"{r['Plot']}"
    )


def main():
    import argparse
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest='command')
    scoreparser = subparsers.add_parser('score')
    scoreparser.add_argument('movie')
    args = parser.parse_args()

    if args.command == 'score':
        result = omdb_query(
            apikey='BanMePlz',
            t=args.movie,
        )
        if result.get('Response') != 'False':
            print(omdb_format(result))

if __name__ == '__main__':
    main()
