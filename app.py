from flask import Flask, render_template, url_for, redirect, session, request, send_from_directory
import json
import os
import uuid
import pandas as pd
import numpy as np
import scipy.spatial.distance
from flask_sockets import Sockets



app = Flask(__name__)
app.config['SECRET_KEY'] = str(uuid.uuid4())
app.debug = os.environ.get('DEBUG', '') == 'TRUE'
sockets = Sockets(app)




@app.route('/')
def index():
    return 'server going'


@sockets.route('/home')
def echo_socket(ws):
    print 'msg received'
    while not ws.closed:
        message = ws.receive()
        print message
        ws.send('i heard you')


@sockets.route('/score')
def calculate_score(ws):
    while not ws.closed:
        message = ws.receive()
        if message is None:
            break
        print 'message received', message
        message = json.loads(message)
        lower = lambda x: [w.lower() for w in x]
        key_words =  lower(message['challenge'])
        input =  lower(message['input'])
        choicesA = lower(message['choicesA'])
        choicesB = lower(message['choicesB'])

        last_words = input[-2:]
        past_words = input[:-2] + ['yokel']

        df_data = pd.read_pickle('resources/mikolov_DIDB.pkl')
        print 'df_data.shape', df_data.shape
        print 'key_words, past_words, input', key_words, past_words, last_words
        scores = insult(df_data, key_words, past_words)
        # print 'unlabeled', list(scores.index)
        # return scores as ranks among choices, in range [0, 1]
        possible_scoresA = [list(scores.index).index(w) / (1.*len(scores))
                                    for w in choicesA]
        possible_scoresB = [list(scores.index).index(w) / (1.*len(scores))
                                    for w in choicesB]
        input_scores = [list(scores.index).index(w) / (1.*len(scores))
                                    for w in last_words]
        scoreA = sorted(possible_scoresA)[::-1].index(input_scores[0]) / float(len(possible_scoresA) - 1.)
        scoreB = sorted(possible_scoresB)[::-1].index(input_scores[1]) / float(len(possible_scoresA) - 1.)

        reply = json.dumps({'scoreA': scoreA, 'scoreB': scoreB})
        ws.send(reply)




def insult(df_data, key_words, past_words):
    """Provide a dataframe with words as index and feature vectors as columns.
    Provide a list of key_words (label +1) and past_words (label -1).
    Do graph laplacian transduction.
    Return weights of words not in key_words + past_words (i.e., unlabeled data).

    After getting unlabeled data, you can look at special words.
    sns.distplot(yu, kde=False)
    sns.distplot(yu.loc[special_words], hist=False, kde=False, rug=True)

    """

    assert(all(w in df_data.index for w in key_words + past_words))

    unlabeled_words = sorted(set(df_data.index) - set(key_words) - set(past_words))

    # key words get label +1, user words get label -1
    Xl = df_data.loc[key_words + past_words]
    yl = [1] * len(key_words) + [-1] * len(past_words)
    Xu = df_data.loc[unlabeled_words]


    metric = lambda X, Y: scipy.spatial.distance.cdist(X, Y, 'cosine')
    yu = graph_transduction(Xl, Xu, yl, metric, k=17)
    yu = pd.Series(yu, index=Xu.index).sort_values()

    return yu


def graph_transduction(Xl, Xu, yl, metric, k=16):
    """Graph transduction with fixed labels, as in pset4 2.2.
    """
    n = len(Xl)
    u = len(Xu)
    m = n + u

    X = np.vstack([Xl.copy(), Xu.copy()])
    W = metric(X, X)

    # mask = np.percentile(W, 100*((m-k-1.)/m), axis=1)
    # W[W<mask] = 0

    mask = np.percentile(W, (100. * k)/m, axis=1)
    W[W>mask] = 100

    L = np.diag(W.sum(axis=1)) - W

    y_bar = L[n:m,:n].dot(yl)
    L_inv = np.linalg.inv(L[n:, n:])
    f_pred = L_inv.dot(y_bar)

    return f_pred



if __name__ == "__main__":
    print 'read df_data.shape', df_data.shape
    from gevent import pywsgi
    from geventwebsocket.handler import WebSocketHandler
    server = pywsgi.WSGIServer(('', 5000), app, handler_class=WebSocketHandler)
    server.serve_forever()
