from flask import Flask, request

app = Flask(__name__)

@app.route("/")
def hello_world():
    print(request.headers)
    return "Hello, World!"
