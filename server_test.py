from flask import Flask, request
from flask.json import jsonify

app = Flask(__name__)


# @app.route("/")
# def hello_world():
#     print(request.headers)
#     return "Hello, World!"
# 
# 
@app.route("/json")
def hello_world_json():
    print(request.headers)
    result = {
        "content": "Hello world",
        "status": "ok",
        "type": "text",
    }
    print(result)
    return jsonify(result)


@app.route("/error")
def hello_world_error():
    print(request.headers)
    result = {
        "message": "Oh non ! Il y a eu une erreur pendant l'execution de l'action",
        "status": "error",
    }
    print(result)
    return jsonify(result)
