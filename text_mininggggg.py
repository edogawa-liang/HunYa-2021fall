# pip install ckiptagger
# pip install tensorflow
# pip install gdown

from ckiptagger import data_utils
data_utils.download_data_gdown("./")


import numpy as np
import time
from ckiptagger import WS, POS, NER

def check(pos_list, word_list, pos_want):
    totalnoun = []
    for pos, words in zip(pos_list, word_list):
        nouns = [word for word, p in zip(words, pos) if p == pos_want]
        totalnoun.append(nouns)     
    return totalnoun

def process_file(file_path, ws, pos, check_function):
    with open(file_path, encoding="utf-8") as file:
        text = file.readlines()
    
    word_sentence_list = ws(text, sentence_segmentation=True, 
                            segment_delimiter_set={",", "。", ":", "?", "!", ";"})
    pos_sentence_list = pos(word_sentence_list)
    result = check_function(pos_sentence_list, word_sentence_list, "Na")
    return result

# 初始化模型
ws = WS("data")
pos = POS("data")
ner = NER("data")

path = "輿情分析資料/發票/"
files = ["bill1_all.txt", "bill1_female.txt", "bill1_male.txt",
         "bill2_all.txt", "bill2_female.txt", "bill2_male.txt",
         "bill3_all.txt", "bill3_female.txt", "bill3_male.txt",
         "bill4_all.txt", "bill4_female.txt", "bill4_male.txt"]

for file_name in files:
    file_path = path + file_name
    time_start = time.time()
    result = process_file(file_path, ws, pos, check)
    time_end = time.time()
    print(f"處理 {file_name} 花費時間：{time_end - time_start} 秒")
    output_path = "輿情分析資料/發票_斷詞完/" + file_name.replace(".txt", ".csv")
    np.savetxt(output_path, result, fmt="%s", delimiter=",", encoding="utf-8")
