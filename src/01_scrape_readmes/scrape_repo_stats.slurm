{
 "cells": [
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "# Scrape repo stats"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 40,
   "metadata": {},
   "outputs": [],
   "source": [
    "import os\n",
    "os.environ['db_user']='zz3hs'\n",
    "os.environ['db_pwd']='zz3hs'"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 71,
   "metadata": {},
   "outputs": [],
   "source": [
    "# load packages \n",
    "import os\n",
    "import psycopg2 as pg\n",
    "from sqlalchemy import create_engine\n",
    "import pandas as pd\n",
    "import requests as r\n",
    "import string \n",
    "import json\n",
    "import base64\n",
    "import urllib.request\n",
    "import itertools \n",
    "from bs4 import BeautifulSoup\n",
    "from dotenv import load_dotenv\n",
    "import warnings\n",
    "warnings.simplefilter(action='ignore', category=FutureWarning)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 72,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "False\n",
      "False\n",
      "True\n",
      "False\n"
     ]
    }
   ],
   "source": [
    "#def string_is_empty(myString):\n",
    "#    if myString and myString.strip():\n",
    "#        return False\n",
    "#    return True\n",
    "\n",
    "def string_is_empty(myString):\n",
    "    if myString == None:\n",
    "         return True\n",
    "    else:\n",
    "        return False\n",
    "\n",
    "print(string_is_empty('  '))\n",
    "print(string_is_empty('')) \n",
    "print(string_is_empty(None))\n",
    "print(string_is_empty('some text'))"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 73,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "4.9k\n",
      "None\n"
     ]
    }
   ],
   "source": [
    "def stars_count(url):\n",
    "    html = r.get(url).text\n",
    "    #soup = BeautifulSoup(html, 'lxml') \n",
    "    soup = BeautifulSoup(html, 'html.parser')\n",
    "    stars_class = \"social-count js-social-count\"\n",
    "    stars_text = soup.find('a', class_ = stars_class)\n",
    "    if string_is_empty(stars_text) == True:\n",
    "        #print(\"empty string\")\n",
    "        #return_value =  print(\"empty string\")\n",
    "        return None\n",
    "    else:\n",
    "        return stars_text.text.strip()\n",
    "\n",
    "\n",
    "url1 = \"https://github.com/tidyverse/ggplot2\"\n",
    "print(stars_count(url1))\n",
    "\n",
    "url2 = \"https://www.geeksforgeeks.org/python-program-to-check-if-string-is-empty-or-not/\"\n",
    "print(stars_count(url2))"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 74,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "<a aria-label=\"4939 users starred this repository\" class=\"social-count js-social-count\" href=\"/tidyverse/ggplot2/stargazers\">\n",
      "      4.9k\n",
      "    </a>\n"
     ]
    },
    {
     "data": {
      "text/plain": [
       "False"
      ]
     },
     "execution_count": 74,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "\n",
    "#example\n",
    "#url = \"https://www.geeksforgeeks.org/python-program-to-check-if-string-is-empty-or-not/\"\n",
    "url = \"https://github.com/tidyverse/ggplot2\"\n",
    "html = r.get(url).text\n",
    "\n",
    "soup = BeautifulSoup(html, 'html.parser')\n",
    "\n",
    "stars_class = \"social-count js-social-count\"\n",
    "stars_text = soup.find('a', class_ = stars_class)\n",
    "\n",
    "print(stars_text)\n",
    "string_is_empty(stars_text) \n",
    "\n",
    "#stars_text == stars_text.empty\n",
    "#stars_text.strip()"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 75,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "/sfs/qumulo/qhome/zz3hs/git/dspg21oss/src/01_scrape_readmes\n",
      "\n"
     ]
    }
   ],
   "source": [
    "myPath = os.getcwd()\n",
    "\n",
    "print(myPath + \"\\n\")"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 76,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/html": [
       "<div>\n",
       "<style scoped>\n",
       "    .dataframe tbody tr th:only-of-type {\n",
       "        vertical-align: middle;\n",
       "    }\n",
       "\n",
       "    .dataframe tbody tr th {\n",
       "        vertical-align: top;\n",
       "    }\n",
       "\n",
       "    .dataframe thead th {\n",
       "        text-align: right;\n",
       "    }\n",
       "</style>\n",
       "<table border=\"1\" class=\"dataframe\">\n",
       "  <thead>\n",
       "    <tr style=\"text-align: right;\">\n",
       "      <th>col_0</th>\n",
       "      <th>count</th>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>primarylanguage</th>\n",
       "      <th></th>\n",
       "    </tr>\n",
       "  </thead>\n",
       "  <tbody>\n",
       "    <tr>\n",
       "      <th>JavaScript</th>\n",
       "      <td>19</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>C#</th>\n",
       "      <td>9</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>TypeScript</th>\n",
       "      <td>9</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>Python</th>\n",
       "      <td>8</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>Java</th>\n",
       "      <td>8</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>Swift</th>\n",
       "      <td>5</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>Go</th>\n",
       "      <td>5</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>PHP</th>\n",
       "      <td>5</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>C++</th>\n",
       "      <td>4</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>Shell</th>\n",
       "      <td>3</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>Objective-C</th>\n",
       "      <td>2</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>Ruby</th>\n",
       "      <td>2</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>Vue</th>\n",
       "      <td>2</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>Jupyter Notebook</th>\n",
       "      <td>1</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>HTML</th>\n",
       "      <td>1</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>R</th>\n",
       "      <td>1</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>G-code</th>\n",
       "      <td>1</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>Rust</th>\n",
       "      <td>1</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>Erlang</th>\n",
       "      <td>1</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>Dockerfile</th>\n",
       "      <td>1</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>CSS</th>\n",
       "      <td>1</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>VBA</th>\n",
       "      <td>1</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>Vim script</th>\n",
       "      <td>1</td>\n",
       "    </tr>\n",
       "    <tr>\n",
       "      <th>C</th>\n",
       "      <td>1</td>\n",
       "    </tr>\n",
       "  </tbody>\n",
       "</table>\n",
       "</div>"
      ],
      "text/plain": [
       "col_0             count\n",
       "primarylanguage        \n",
       "JavaScript           19\n",
       "C#                    9\n",
       "TypeScript            9\n",
       "Python                8\n",
       "Java                  8\n",
       "Swift                 5\n",
       "Go                    5\n",
       "PHP                   5\n",
       "C++                   4\n",
       "Shell                 3\n",
       "Objective-C           2\n",
       "Ruby                  2\n",
       "Vue                   2\n",
       "Jupyter Notebook      1\n",
       "HTML                  1\n",
       "R                     1\n",
       "G-code                1\n",
       "Rust                  1\n",
       "Erlang                1\n",
       "Dockerfile            1\n",
       "CSS                   1\n",
       "VBA                   1\n",
       "Vim script            1\n",
       "C                     1"
      ]
     },
     "execution_count": 76,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "# connect to the database, download data, limit to top 5000 repos that have the highest number of commits\n",
    "connection = pg.connect(host = 'postgis1', database = 'sdad', \n",
    "                        user = os.environ.get('db_user'), \n",
    "                        password = os.environ.get('db_pwd'))\n",
    "\n",
    "raw_slug_data = '''SELECT * FROM gh_2007_2020.repos LIMIT 100'''\n",
    "\n",
    "# convert to a dataframe, show how many missing we have (none)\n",
    "raw_slug_data = pd.read_sql_query(raw_slug_data, con=connection)\n",
    "raw_slug_data.head()\n",
    "\n",
    "language_table = pd.crosstab(index =raw_slug_data['primarylanguage'], columns = 'count')\n",
    "language_table.sort_values(by=['count'], ascending=False)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 77,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "10"
      ]
     },
     "execution_count": 77,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "raw_slug_data = '''SELECT * FROM gh_2007_2020.repos WHERE  primarylanguage='R' LIMIT 10'''\n",
    "\n",
    "# convert to a dataframe, show how many missing we have (none)\n",
    "raw_slug_data = pd.read_sql_query(raw_slug_data, con=connection)\n",
    "raw_slug_data.head()\n",
    "\n",
    "index = raw_slug_data.index\n",
    "len(index)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 78,
   "metadata": {},
   "outputs": [],
   "source": [
    "#slugs = [\"brandonleekramer/diversity\", \"uva-bi-sdad/oss-2020\", \"facebook/react\"] #test data \n",
    "slugs = raw_slug_data.slug.tolist()\n",
    "#print(*slugs)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 79,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "['6', '31', '14', '12', '10', '7', '6', '3', '1', '1']\n"
     ]
    }
   ],
   "source": [
    "#slugs_example = slugs[1:10]\n",
    "#print(slugs_example)\n",
    "\n",
    "#star = []\n",
    "#for slug in slugs_example:\n",
    "#    url =f'https://github.com/{slug}/'\n",
    "#    star.append(stars_count(url))\n",
    "\n",
    "star = []\n",
    "for slug in slugs:\n",
    "    url =f'https://github.com/{slug}/'\n",
    "    star.append(stars_count(url))\n",
    "    \n",
    "print(star)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 80,
   "metadata": {},
   "outputs": [],
   "source": [
    "raw_slug_data[\"stars\"] = star"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 81,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "                                 id     spdx                       slug  \\\n",
      "0  MDEwOlJlcG9zaXRvcnkyMTc1NjgxMzc=  GPL-3.0     JamesGallant/ProVision   \n",
      "1  MDEwOlJlcG9zaXRvcnk2NTQ1NDk0NA==      MIT  likelet/PlotAppForTBtools   \n",
      "2  MDEwOlJlcG9zaXRvcnkxNDE1OTg2OTg=  GPL-3.0         ajwills72/catlearn   \n",
      "3  MDEwOlJlcG9zaXRvcnk2NDM2NjM0NA==      MIT            wdecoster/DEA.R   \n",
      "4  MDEwOlJlcG9zaXRvcnk2NTQyODA2MQ==      MIT         clarkfitzg/rddlist   \n",
      "\n",
      "            createdat                                        description  \\\n",
      "0 2019-10-25 15:55:44             Shiny app for proteomics data analysis   \n",
      "1 2016-08-11 09:01:32                  A shiny app for TBtools data plot   \n",
      "2 2018-07-19 15:30:04  Formal Psychological Models of Categorization ...   \n",
      "3 2016-07-28 05:34:39  Script to automate differential expression ana...   \n",
      "4 2016-08-11 01:25:34  Implements some methods of an R list as a Spar...   \n",
      "\n",
      "  primarylanguage                                        branch  commits  \\\n",
      "0               R  MDM6UmVmMjE3NTY4MTM3OnJlZnMvaGVhZHMvbWFzdGVy       12   \n",
      "1               R  MDM6UmVmNjU0NTQ5NDQ6cmVmcy9oZWFkcy9tYXN0ZXI=       22   \n",
      "2               R  MDM6UmVmMTQxNTk4Njk4OnJlZnMvaGVhZHMvbWFzdGVy       85   \n",
      "3               R  MDM6UmVmNjQzNjYzNDQ6cmVmcy9oZWFkcy9tYXN0ZXI=       87   \n",
      "4               R  MDM6UmVmNjU0MjgwNjE6cmVmcy9oZWFkcy9tYXN0ZXI=       22   \n",
      "\n",
      "                 asof status stars  \n",
      "0 2021-01-03 14:43:49   Done     6  \n",
      "1 2021-01-03 06:13:17   Done    31  \n",
      "2 2021-01-03 14:01:16   Done    14  \n",
      "3 2021-01-03 06:13:17   Done    12  \n",
      "4 2021-01-03 06:13:17   Done    10  \n"
     ]
    }
   ],
   "source": [
    "print(raw_slug_data.head())"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 83,
   "metadata": {},
   "outputs": [],
   "source": [
    "raw_slug_data.to_csv(r'/home/zz3hs/git/dspg21oss/output/repo_slugs_R_stars.csv', index = False)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 84,
   "metadata": {},
   "outputs": [],
   "source": [
    "df = pd.read_csv('/home/zz3hs/git/dspg21oss/output/repo_slugs_R_stars.csv')"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 85,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "Index(['id', 'spdx', 'slug', 'createdat', 'description', 'primarylanguage',\n",
      "       'branch', 'commits', 'asof', 'status', 'stars'],\n",
      "      dtype='object')\n",
      "         commits      stars\n",
      "count  10.000000  10.000000\n",
      "mean   45.300000   9.100000\n",
      "std    35.521668   8.849984\n",
      "min     3.000000   1.000000\n",
      "25%    16.750000   3.750000\n",
      "50%    30.000000   6.500000\n",
      "75%    84.250000  11.500000\n",
      "max    87.000000  31.000000\n",
      "(10, 11)\n"
     ]
    },
    {
     "data": {
      "text/plain": [
       "<bound method DataFrame.info of                                  id     spdx                       slug  \\\n",
       "0  MDEwOlJlcG9zaXRvcnkyMTc1NjgxMzc=  GPL-3.0     JamesGallant/ProVision   \n",
       "1  MDEwOlJlcG9zaXRvcnk2NTQ1NDk0NA==      MIT  likelet/PlotAppForTBtools   \n",
       "2  MDEwOlJlcG9zaXRvcnkxNDE1OTg2OTg=  GPL-3.0         ajwills72/catlearn   \n",
       "3  MDEwOlJlcG9zaXRvcnk2NDM2NjM0NA==      MIT            wdecoster/DEA.R   \n",
       "4  MDEwOlJlcG9zaXRvcnk2NTQyODA2MQ==      MIT         clarkfitzg/rddlist   \n",
       "5  MDEwOlJlcG9zaXRvcnk3NzIyOTAwNA==      MIT             tdepke/CluMSID   \n",
       "6  MDEwOlJlcG9zaXRvcnk2Mzg0NDE4MA==      MIT      kinow/nz-shiny-server   \n",
       "7  MDEwOlJlcG9zaXRvcnk1OTI2MTMyMA==      MIT  Keith-Godwin/QG-STEC-plus   \n",
       "8  MDEwOlJlcG9zaXRvcnk2ODYzMTMzNg==      MIT             TKoscik/tkmisc   \n",
       "9  MDEwOlJlcG9zaXRvcnk3NTY1MTkzNA==      MIT     CancerRxGene/gdscDataR   \n",
       "\n",
       "             createdat                                        description  \\\n",
       "0  2019-10-25 15:55:44             Shiny app for proteomics data analysis   \n",
       "1  2016-08-11 09:01:32                  A shiny app for TBtools data plot   \n",
       "2  2018-07-19 15:30:04  Formal Psychological Models of Categorization ...   \n",
       "3  2016-07-28 05:34:39  Script to automate differential expression ana...   \n",
       "4  2016-08-11 01:25:34  Implements some methods of an R list as a Spar...   \n",
       "5  2016-12-23 14:02:17  Clustering of MS² Spectra for Metabolite Ident...   \n",
       "6  2016-07-21 06:46:17                                  NZ R-Shiny Server   \n",
       "7  2016-05-20 03:16:24                                                NaN   \n",
       "8  2016-09-19 17:53:20                TKoscik's Miscellaneous R Functions   \n",
       "9  2016-12-05 17:59:29  Package to be used in conjunction with the out...   \n",
       "\n",
       "  primarylanguage                                        branch  commits  \\\n",
       "0               R  MDM6UmVmMjE3NTY4MTM3OnJlZnMvaGVhZHMvbWFzdGVy       12   \n",
       "1               R  MDM6UmVmNjU0NTQ5NDQ6cmVmcy9oZWFkcy9tYXN0ZXI=       22   \n",
       "2               R  MDM6UmVmMTQxNTk4Njk4OnJlZnMvaGVhZHMvbWFzdGVy       85   \n",
       "3               R  MDM6UmVmNjQzNjYzNDQ6cmVmcy9oZWFkcy9tYXN0ZXI=       87   \n",
       "4               R  MDM6UmVmNjU0MjgwNjE6cmVmcy9oZWFkcy9tYXN0ZXI=       22   \n",
       "5               R  MDM6UmVmNzcyMjkwMDQ6cmVmcy9oZWFkcy9tYXN0ZXI=       82   \n",
       "6               R  MDM6UmVmNjM4NDQxODA6cmVmcy9oZWFkcy9tYXN0ZXI=       87   \n",
       "7               R  MDM6UmVmNTkyNjEzMjA6cmVmcy9oZWFkcy9tYXN0ZXI=       15   \n",
       "8               R  MDM6UmVmNjg2MzEzMzY6cmVmcy9oZWFkcy9tYXN0ZXI=       38   \n",
       "9               R  MDM6UmVmNzU2NTE5MzQ6cmVmcy9oZWFkcy9tYXN0ZXI=        3   \n",
       "\n",
       "                  asof status  stars  \n",
       "0  2021-01-03 14:43:49   Done      6  \n",
       "1  2021-01-03 06:13:17   Done     31  \n",
       "2  2021-01-03 14:01:16   Done     14  \n",
       "3  2021-01-03 06:13:17   Done     12  \n",
       "4  2021-01-03 06:13:17   Done     10  \n",
       "5  2021-01-03 06:13:17   Done      7  \n",
       "6  2021-01-03 06:13:17   Done      6  \n",
       "7  2021-01-03 06:13:17   Done      3  \n",
       "8  2021-01-03 06:13:17   Done      1  \n",
       "9  2021-01-03 06:13:17   Done      1  >"
      ]
     },
     "execution_count": 85,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "print(df.columns)\n",
    "print(df.describe())\n",
    "print(df.shape)\n",
    "df.info"
   ]
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python [conda env:.conda-crystal]",
   "language": "python",
   "name": "conda-env-.conda-crystal-py"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.7.9"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 4
}
