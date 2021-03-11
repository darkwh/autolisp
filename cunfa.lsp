(defun c:test(/)
	(vl-load-com) 
	;读取系统变量
	(setq QAFLAGS (getvar "QAFLAGS"))
	
	;设置系统变量(可以炸开选择集所有对象)
	(setvar "QAFLAGS" 1)
	
 	;选取区域
 	(setq pt1 (getpoint "First Pt:"))
  	(setq pt2 (getcorner pt1))
	(setq SS (ssget "_c"  pt1 pt2))
	;获取全部中心线
	(setq centerLines (ssget "_c"  pt1 pt2 '((8 . "center_line"))))
	;(alert (itoa (sslength centerLines)))
  	;(alert (itoa (sslength SS)))
	(setq N (sslength SS)
		I 0
	)
	(setq insert_list (ssadd))
	(repeat N
		(progn	
			(setq ENAME	(ssname SS I)
				ALIST	(entget ENAME)
				TT	(cdr (assoc 0 ALIST))
			)
		)
		(if (= TT "INSERT")
      	   (progn
				;炸开图块
				;将图块放到一个新的表中最后一起炸,方便还原
				;(command "explode" ENAME)
				;(cons ENAME insert_list)
				(ssadd ENAME insert_list)
      	    )
        )
		;;;(alert (itoa I))
		(setq I (1+ I))
    )
	;(alert (itoa (sslength insert_list)))
	(if (>= (sslength insert_list))
		(progn
			;进行炸块操作
			(print "EXPLODE start")
			(command "EXPLODE" insert_list "")
			;炸开块之后重新选择区域对象,且获取所有交点集合(获取的是二维表)
			(setq all_inters (reSelect pt1 pt2 centerLines))
			;撤销之前的炸块操作
			(command "U")
			;TODO 循环遍历进行标注作业
			(setq A_N (length all_inters)
				A_I 0
			)
			(repeat A_N
				(progn
					(setq sub_inters (nth A_I all_inters)
						S_N (length sub_inters)
						S_I 0
					)
					(if (> S_N 1)
						(progn
							(repeat S_N
								(progn
									(if (/= S_I (- S_N 1))
										(progn
											(setq l1 (nth S_I sub_inters))
											(setq l2 (nth (+ S_I 1) sub_inters))
											(setq p1 (list (nth 0 l1) (nth 1 l1)))
											(setq p2 (list (nth 0 l2) (nth 1 l2)))
											(executeDimaligned p1 p2)
										)
									)
								)
								(setq S_I (1+ S_I))
							)
						)
					)
				)
				(setq A_I (1+ A_I))
			)
		)
	)
	;恢复系统变量
	(setvar "QAFLAGS" QAFLAGS)
)

;获取拆解块后的所有非图块类型和中心线的交点集合(这里是二维表)
(defun reSelect(pt1 pt2 centerLines / all_inters)
	(vl-load-com)
	(print "reSelect start")
	(setq all_pel (ssget "_c"  pt1 pt2))
	(setq N1 (sslength centerLines)
		I1 0
	)
	(setq all_inters (list))
	(repeat N1
		(progn	
			(setq N2 (sslength all_pel)
				J 0
				center_line (ssname centerLines I1)
				inters_list (list)
			)
			(repeat N2
				(progn
					(setq pel (ssname all_pel J)
						ALIST	(entget pel)
						TT	(cdr (assoc 8 ALIST))
					)
					(if (= TT "¥¹¥¿¥åɤf")
						(progn
							(setq temp (acet-geom-intersectwith center_line pel 0)
								N3 (length temp)
								K 0
							)
							(repeat N3
								(progn
									(setq inters_list (cons (nth K temp) inters_list))
								)
								(setq K (1+ K))
							)
						)
					)
				)
				(setq J (1+ J))
			)
			;TODO 这里需对inters_list进行冒泡排序
			(setq newsort 
				(vl-sort inters_list (function (lambda (e1 e2)
					(< (car e1) (car e2))
					))
				)
			)
			(print newsort)
			(setq all_inters (cons inters_list all_inters))
		)
		(setq I1 (1+ I1))
	)
	(print "all_inters length is")
	(print (length all_inters))
	;(print all_inters)
	all_inters
)

(defun executeDimaligned(pt1 pt2)
	(setq p3x (* (+ (car p1) (car p2)) 0.5))
	(setq p3y (- (cadr p2) 800))
	(setq p3 (list p3x p3y))
	(command "dimaligned" pt1 pt2 p3)
)