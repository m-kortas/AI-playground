import random

import cv2


def change_brightness(img):
    value = random.randint(-50, 50)
    return cv2.add(img, value)


def flip(img):
    value = random.randint(-1, 1)
    return cv2.flip(img, value)


def blur(img):
    ksize = random.randint(3, 7)
    return cv2.blur(img, (ksize, ksize))


def main():
    filename = "Acc-optima.png"
    for i in range(20):
        img = cv2.imread(filename)
        if random.random() < 0.5:
            img = change_brightness(img)
        if random.random() < 0.3:
            img = flip(img)
        if random.random() < 0.8:
            img = blur(img)
        cv2.imwrite(str(i) + filename, img)


if __name__ == '__main__':
    main()