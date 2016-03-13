# -*- coding: utf-8 -*-
"""
Created on Wed Dec 30 22:49:43 2015

@author: yl
"""

import math
import copy
import pdb

def del_dot(x):  # int 转换为 float型 并处理计算机的浮点错误
    if isinstance(x, int):
        return float(x)
    if round(x, 4) == round(x, 14):
        x = round(x, 4)
    return x

class Myla(object):


    def __init__(self, lis):
        self.lis = map(lambda y:map(del_dot, y), lis)  # 转换为浮点 并处理计算机的浮点错误

    def __str__(self):
        strs = '\n'
        for row in self.lis:
            strs += '|'
            for elem in row:
                strr = ('%.2f '%(elem)).rjust(10,' ')[-8:]
                strs += strr
            strs += '|\n'
        strs = strs[:-1] + '  %dx%d\n' % (len(self.lis),len(self.lis[0]))
        return strs

    __repr__ = __str__

    @property
    def shape(self):
        return len(self.lis[0]), len(self.lis)

    def __add__(self, la):
        if self.shape != la.shape:
            print '[ERORR]The shape of %s and %s are not same!' % (self.lis, la.lis)
            return
        relut = []

        def list_add(a, b):
            c = []
            lenn = len(a)
            for i in range(0, lenn):
                c += [a[i] + b[i]]
            return c

        lenn = len(la.lis)
        for i in range(0, lenn):
            relut += [list_add(self.lis[i], la.lis[i])]
        return Myla(relut)

    def t(self):
        l = self.lis
        listt = [self.shape[1]]
        for i in range(0, self.shape[0]):
            for j in range(0, self.shape[1]):
                listt += [l[j][i]]


        def list_to_la(listt):   #复制函数 str to la() 的代码
            lenn = len(listt) - 1
            if lenn % listt[0] != 0:
                print '[ERORR] 输入数据不是 %d 的整数倍!' % listt[0]
                return
            lis = []
            lisrow = []
            count = 0
            for elem in listt[1:]:
                lisrow += [elem]
                count += 1
                if count == listt[0]:
                    count = 0
                    lis += [lisrow]
                    lisrow = []
            return Myla(lis)
        return list_to_la(listt)

    def __mul__(self, la):

        def num_mul(l, num):

            la = Myla(copy.deepcopy(l.lis))
            lenn = len(la.lis)
            lennn = len(la.lis[0])
            for i in range(0, lenn):
                for j in range(0, lennn):
                    la.lis[i][j] *= num
            return la

        def la_mul(la1, la2):
            def row_mul(la1, la2, i, j):
                summ = 0
                k = 0
                for row in la1.lis[i]:
                    summ += row * la2.lis[k][j]
                    k += 1
                return summ

            shape1 = la1.shape
            shape2 = la2.shape
            if shape1[0] != shape2[1]:
                print '[ERORR]左矩阵的列数和右矩阵的行数不相等!'
                return
            lis = []
            for j in range(0, shape2[0]):
                lisrow = []
                for i in range(0, shape1[1]):
                    lisrow += [row_mul(la1, la2, i, j)]
                lis += [lisrow]

            return Myla(lis)
        if isinstance(la, int):
            print la
            return num_mul(self, la)
        else:
            return la_mul(self, la)

    def inverse(self):  # 矩阵的逆
        listt = copy.deepcopy(self.lis)
        n = len(listt)
        min_a = e_list(n)
        for j in range(0, n-1):
            for i in range(j, n):  # 找到一个非零开头的row
                if listt[i][j] == 0:
                    exchange_row(j, i+1, listt)
                    exchange_row(j, i+1, min_a)
                else:
                    break
            k = 1/listt[j][j]
            k_row(j, k, listt)  # 将第一个非零变为1
            k_row(j, k, min_a)
            for i in range(j+1, n):  # 将下面的n-j排的j项变为0
                k = - listt[i][j] / listt[j][j]  # k=-i/j
                i_add_kj(i, j, k, listt)  # i=j*k+i
                i_add_kj(i, j, k, min_a)
        k = 1/listt[n-1][n-1]
        k_row(n-1, k, listt)  # 最后一项变为1
        k_row(n-1, k, min_a)
        for i in range(0, n-1):  # 化为简单阶梯矩阵
            for j in range(i+1, n):
                k = - listt[i][j] / listt[j][j]
                i_add_kj(i, j, k, listt)
                i_add_kj(i, j, k, min_a)
        return Myla(min_a)


    @property
    def r(self):  # 矩阵的秩
        listt = copy.deepcopy(self.lis)
        n = len(listt)
        i_now = 0  # i 的起始
        for j in range(0, n):
            tag = 0  # 0表示此cloum为0 tag = 1 表示找到非零项
            for i in range(i_now, n):  # 找到一个非零开头的row
                if listt[i][j] != 0:
                    exchange_row(i_now, i, listt)
                    tag = 1
                    break
            if tag == 0:
                continue
            for i in range(i_now+1, n):  # 将下面的n-j排的j项变为0
                k = - listt[i][j] / listt[i_now][j]  # k=-i/j
                i_add_kj(i, j, k, listt)  # i=j*k+i
            if tag == 1:
                i_now += 1
        return i_now

    @property
    def det(self):  # 求行列式的值
        summ = 0
        listt = self.lis
        n = len(listt)
        def get_muli(j, listt, tag=1):
            n = len(listt)
            m = 1
            if tag == 1:
                for i in range(0, n):
                    m *= listt[i][(j+i)%n]
            else:
                m = -1
                for i in range(0, n):
                    m *= listt[i][(j-i)%n]
            return m
        for i in range(0, n):
            summ = summ + get_muli(i, listt) + get_muli(i, listt, -1)
        return summ

def m2():
    strr='3 1 -2 2 2 5 6 2 5 6 '
    strr1 = '1 2 1 5'
    la1 = str_to_la(strr1)
    la = str_to_la(strr)
    l = Myla(e_list(5, 2))
    print la ,la.r



def f_in_list(f, listt):
    lenn = len(listt)
    for i in range(0,lenn):
        listt[i] = f(listt[i])  # 得返回再赋值才能改变list

def exchange_row(i, j , listt):
    if i == j:
        return
    listt[i], listt[j] = (listt[j], listt[i])

def e_list(n, x=1.0):
    lis = []
    x = float(x)
    for i in range(0, n):
        lis += [[0.0] * n]
        lis[i][i] = x
    return lis

def i_add_kj(row, row2, k, listt):  # row=row2*k+row
    lenn = len(listt[row])
    for j in range(0,lenn):
        listt[row][j] += listt[row2][j] * k
        if round(listt[row][j], 14) == round(listt[row][j], 4):
            listt[row][j] = round(listt[row][j], 4)

def k_row(i, k, listt):
    f_in_list(lambda x:x*k, listt[i])









def str_to_la(strr):
    listt = strr.split()
    listt = map(lambda x:float(x), listt)
    lenn = len(listt) - 1
    if lenn % listt[0] !=0:
        print '[ERORR] 输入数据不是 %d 的整数倍!'%listt[0]
        return
    lis = []
    lisrow = []
    count = 0
    for elem in listt[1:]:

        lisrow += [elem]
        count += 1
        if count == listt[0]:
            count = 0
            lis += [lisrow]
            lisrow = []
    #print lis
    return Myla(lis)

def cin_la():
    print('请输入矩阵，格式为第一个数字为列数，接着依次输入矩阵，以空格分开：')
    strr = raw_input()
    x = str_to_la(strr)
    print '您输入的矩阵是：',
    return x

def main1():
    lis = [[1, 0], [0, 1]]
    lis1 = [[1, 2, 5], [6, 5, 9]]
    lis2 = [[1, 3], [6, 5]]


    la = Myla(lis)
    la1 = Myla(lis1)

    la2 = la1.t()
    print la1
    print la2.t




def m3():
    lis = [[1, 2], [3, 1]]
    lis1 = [[3, 2, 5], [6, 5, 9],[3, 5, 4]]
    lis2 = [[1, -1, -1, 1], [3, 0, -3, 4], [3, -2, 2, -1], [-1, 1, 2, -2]]


    la1 = Myla(lis2)
    la11= la1.inverse()

    print '=====\n',la1
    print la11
    print la1.t() * la11.t()
m2()





cin_la()













