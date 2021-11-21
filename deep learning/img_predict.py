import cv2
from flask import Flask, request
import numpy as np
import io
from keras.applications import ResNet50, VGG16
from keras.applications.imagenet_utils import decode_predictions


app = Flask(__name__)

model = None


def img_from_file(photo):
    in_memory_file = io.BytesIO()
    photo.save(in_memory_file)
    data = np.fromstring(in_memory_file.getvalue(), dtype=np.uint8)
    return cv2.imdecode(data, cv2.IMREAD_COLOR)

@app.route("/")
def main():
    return "Infoshare Academy"


@app.route("/classify", methods=["POST"])
def classify():
    global model

    if model is None:
        model = ResNet50()

    photo = request.files["img"]
    img = img_from_file(photo)
    img = cv2.resize(img, (224, 224), interpolation=cv2.INTER_AREA)
    results = model.predict(np.array([img]))
    result = decode_predictions(results)[0]

    return str("\n").join([x[1] for x in result])


if __name__ == '__main__':
    app.run()
