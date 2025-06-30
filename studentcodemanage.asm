assume cs:code,ds:data
data segment
; Define student structure
student_struct struc
    student_name db 20 dup (?)        ; 名字, max 20 chars
    student_id db 10 dup (?)          ; ID, max 10 chars
    homework_score dw 8 dup (0)       ; 8次小作业成绩 (now word size)
    project_score dw 0                ; 大作业成绩 (now word size)
    usual_score dw 0                  ; 平时成绩 (now word size)
    final_score dw 0                  ; 最终成绩 (now word size)
student_struct ends

students_array student_struct 100 dup (<>)  ; 学生数组
stu_count db 0                                  ; 学生个数
; Query related
input_name_buffer db 20 dup (?), '$'        ; 存储姓名（临时）
input_id_buffer db 10 dup (?), '$'         ; 存储id

; Sorting related
temp_student student_struct <>              ; Temp student for swapping

; Statistics related
score_seg90_100 db 0                        ; 90 - 100 score range count
score_seg80_89 db 0                         ; 80 - 89 score range count
score_seg60_79 db 0                         ; 60 - 79 score range count
score_seg60_bel db 0                        ; Below 60 score range count
total_score dw 0                            ; Total score sum (now dword)
average_score dw 0                          ; Average score (now word)
highest_score dw 0                          ; Highest score (now word)
lowest_score dw 100                         ; Lowest score (init to 100)

; Menu messages
menu_message db 0dh,0ah
			 db '------Student Grade System------', 0dh, 0ah
             db '1. Input student info', 0dh, 0ah
             db '2. Query student', 0dh, 0ah
             db '3. Score ranking', 0dh, 0ah
             db '4. Statistics', 0dh, 0ah
             db '5. Exit', 0dh, 0ah
             db 'Please choose(1-5): $'
;功能1数据
homework_prompt db 0dh, 0ah, 'Enter homework score #x (0-100): $'
project_prompt db 0dh, 0ah, 'Enter project score (0-100): $'
max_students_msg db 0dh, 0ah, 'Maximum students reached (100)!$'
input_name_prompt db 0dh, 0ah, 'Enter student name (max 20 chars): $'
input_id_prompt db 0dh, 0ah, 'Enter student ID (max 10 chars): $'
homework_count db 8  ; 小作业次数
current_hw_count db 0 ; 当前小作业计数
invalid_score_msg db 0dh, 0ah, 'Invalid score! Must be 0-100. Please re-enter: $'
;功能2查询
check_menu_msg      db 0dh,0ah,'Search by:',0dh,0ah
                       db '1. Name',0dh,0ah
                       db '2. ID',0dh,0ah
                       db 'Enter choice (1-2): $'
    
    invalid_choice_msg  db 0dh,0ah,'Invalid choice! Please enter 1 or 2.$'
    
    ; 输入缓冲区
    search_buffer       db 20 dup(?), '$'   ; 存储查询输入
not_found_msg db 0dh, 0ah, 'Student not found!$'
student_info_header db 0dh, 0ah, '----- Student Info -----', 0dh, 0ah, '$'
name_display db 'Name: $'
id_display db 0dh, 0ah, 'ID: $'
usual_display db 0dh, 0ah, 'Usual score: $'
project_display db 0dh, 0ah, 'Project score: $'
final_display db 0dh, 0ah, 'Final score: $'
;功能3排名
sort_order db 0                  ; 0=升序, 1=降序
invalid_sort_msg db 0dh,0ah,'Please enter 0 or 1.(asc/desc)$'

rank_num db 0                    ; 当前排名
rank_display db '  Rank: $'
sort_display_header db '----- Score Ranking -----', 0dh, 0ah, '$' ; 添加此行定义
sort_display_line db '       name id usual project final', 0dh, 0ah, '$' 
space_char db ' ', '$'  ; 注意要以'$'结尾
;功能4统计
statistics_header db 0dh, 0ah, '----- Statistics -----', 0dh, 0ah, '$'
seg90_100 db '90 - 100 range: $'
seg80_89 db 0dh, 0ah, '80 - 89 range: $'
seg60_79 db 0dh, 0ah, '60 - 79 range: $'
seg_below60 db 0dh, 0ah, 'Below 60 range: $'
avg_score db 0dh, 0ah, 'Average: $'
max_score db 0dh, 0ah, 'Highest: $'
min_score db 0dh, 0ah, 'Lowest: $'



no_students_msg db 0dh, 0ah, 'No student data available!$'
goodbye db 0dh, 0ah, 'System exited successfully!$'
newline db 0dh, 0ah, '$'
choice_output db 0dh, 0ah, 'You selected option: $'
data ends


code segment
start:
    mov ax, data      
    mov ds, ax

main_loop:
    call display_menu
    call get_user_choice
    call execute_choice
    jmp main_loop

;-----显示主界面-----
display_menu proc near
    mov dx, offset menu_message
    mov ah, 09h
    int 21h
    ret
display_menu endp

;-----获得输入选择结果-----
get_user_choice proc near
input_again:
    mov ah, 01h     ; Get user input
    int 21h
    
    ; 查看是否是1-5之间
    cmp al, '1'
    jb invalid_input
    cmp al, '5'
    ja invalid_input
    ret
    
invalid_input:
    ; 错误信息
    mov dx, offset newline
    mov ah, 09h
    int 21h
    mov dx, offset menu_message
    mov ah, 09h
    int 21h
    jmp input_again
get_user_choice endp

;----处理选择----
execute_choice proc near
    
    push ax 
    mov dx, offset choice_output
    mov ah, 09h
    int 21h
    
    pop ax 
    mov dl, al
    mov ah, 02h 
    int 21h
    
    mov dx, offset newline
    mov ah, 09h
    int 21h
    
    ; 处理选择
    cmp al, '1'
    je call_input_student
    cmp al, '2'
    je call_check_student
    cmp al, '3'
    je call_score_rank
    cmp al, '4'
    je call_grade_stats
    cmp al, '5'
    je call_exit
    call_input_student:
		call input_student
		ret
	call_check_student:
		call check_stu
		ret
		
	call_score_rank:
	   call score_rank
		ret
		
	call_grade_stats:
		call grade_stats
		ret
	call_exit:
		call exit_program
		ret
execute_choice endp

;------------------------function1 设置学生信息--------------------------------------------------
input_student proc near
    ; 检查是否已达到最大学生数
    cmp [stu_count], 100
    jb can_input
    mov dx, offset max_students_msg
    mov ah, 09h
    int 21h
    ret
    
can_input:
    ; 计算当前学生在数组中的位置
    mov al, [stu_count]
    mov bl, size student_struct
    mul bl
    lea si, students_array
    add si, ax  ; SI指向当前学生结构
    
    ; 输入学生姓名
    mov dx, offset input_name_prompt
    mov ah, 09h
    int 21h
    mov cx, 20  ; 最大20字符
    lea di, [si].student_name
    call read_string
    
    ; 输入学生ID
    mov dx, offset input_id_prompt
    mov ah, 09h
    int 21h
    mov cx, 10  ; 最大10字符
    lea di, [si].student_id
    call read_string
    
    ; 输入8次小作业成绩
    mov [current_hw_count], 0
hw_input_loop:
    ; 显示提示信息
    mov dx, offset homework_prompt
    mov ah, 09h
    int 21h
    
    ; 显示当前是第几次作业
    mov al, [current_hw_count]
    add al, '1'
    mov dl, al
    mov ah, 02h
    int 21h
    mov dl, ':'
    int 21h
    mov dl, ' '
    int 21h
    
    ; 读取成绩(0-100)
    call read_score
    ; 存储成绩
    mov bx, 0
    mov bl, [current_hw_count]
    shl bx, 1 ; multiply by 2 for word size
    mov [si].homework_score[bx], ax
    
    ; 更新计数器
    inc [current_hw_count]
    mov al, [current_hw_count]
    cmp al, [homework_count]
    jb hw_input_loop
    
    ; 输入大作业成绩
    mov dx, offset project_prompt
    mov ah, 09h
    int 21h
    call read_score
    mov [si].project_score, ax
    
    ; 计算平时成绩(小作业平均分)
    call calculate_usual_score
    ; 计算最终成绩
    call calculate_final_score
    ; 增加学生计数
    inc [stu_count]
    
    ; 显示新行并返回
    mov dx, offset newline
    mov ah, 09h
    int 21h
    ret
input_student endp

;----- 计算平时成绩(小作业平均分) -----
calculate_usual_score proc near
    push cx
    push bx
    push dx
    
    xor ax, ax      ; clear ax (sum)
    xor dx, dx      ; clear dx (high word of sum)
    xor bx, bx      ; index
    mov cx, 8
    
sum_hw_scores:
    add ax, [si].homework_score[bx]
    adc dx, 0       ; add carry to dx if overflow
    add bx, 2       ; increment by 2 for word size
    loop sum_hw_scores
    
    ; Now divide dx:ax by 8
    mov cx, 8
    div cx          ; ax = quotient, dx = remainder
    
    ; Rounding
    cmp dx, 4       ; if remainder >= 4, round up
    jb no_round_usual
    inc ax
    
no_round_usual:
    mov [si].usual_score, ax
    
    pop dx
    pop bx
    pop cx
    ret
calculate_usual_score endp

;----- 计算最终成绩 -----
calculate_final_score proc near
    push bx
    push cx
    push dx
    
    ; 计算 usual_score * 0.4
    mov ax, [si].usual_score
    mov bx, 4       ; 0.4 = 4/10
    mul bx          ; result in dx:ax
    mov bx, 10
    div bx          ; ax = result, dx = remainder
    ; Rounding
    cmp dx, 5       ; if remainder >= 5, round up
    jb no_round1
    inc ax
no_round1:
    mov cx, ax      ; save first part
    
    ; 计算 project_score * 0.6
    mov ax, [si].project_score
    mov bx, 6       ; 0.6 = 6/10
    mul bx          ; result in dx:ax
    mov bx, 10
    div bx          ; ax = result, dx = remainder
    ; Rounding
    cmp dx, 5       ; if remainder >= 5, round up
    jb no_round2
    inc ax
no_round2:
    
    ; 两部分相加
    add cx, ax
    
    ; 确保不超过100分
    cmp cx, 100
    jbe store_final
    mov cx, 100
    
store_final:
    mov [si].final_score, cx
    pop dx
    pop cx
    pop bx
    ret
calculate_final_score endp

;----- 读取字符串 -----
; 输入: CX=最大长度, DI=存储地址
read_string proc near
    push ax
    push cx
    push di
    
    mov ah, 01h
    mov byte ptr [di], 0  ; 初始化为空
read_char:
    int 21h
    cmp al, 0dh  ; 回车结束
    je read_done
    mov [di], al
    inc di
    loop read_char
    
read_done:
    mov byte ptr [di], '$'  ; 添加字符串结束符
    pop di
    pop cx
    pop ax
    ret
read_string endp

;----- 读取成绩(0-100) -----
; 输出: AX=读取的成绩
read_score proc near
    push bx
    push cx
    push dx
    
    xor bx, bx      ; clear bx to store result
    
read_first_digit:
    mov ah, 01h
    int 21h
    cmp al, 0dh     ; check for enter (single digit)
    je score_done
    
    ; Check if digit
    cmp al, '0'
    jb read_first_digit
    cmp al, '9'
    ja read_first_digit
    
    ; Convert to number and store
    sub al, '0'
    mov bl, al      ; store first digit
    
    ; Read second digit
    mov ah, 01h
    int 21h
    cmp al, 0dh     ; check for enter (single digit)
    je single_digit
    
    ; Check if digit
    cmp al, '0'
    jb read_first_digit
    cmp al, '9'
    ja read_first_digit
    
    ; Convert to number
    sub al, '0'
    mov bh, al      ; store second digit
    
    ; Calculate value: bl*10 + bh
    mov al, bl
    mov cl, 10
    mul cl
    add al, bh
    mov bl, al
    
    ; Check if we need to read third digit (for 100)
    cmp bl, 10      ; if first two digits were "10"
    jne check_score_range
    
    ; Read third digit
    mov ah, 01h
    int 21h
    cmp al, '0'     ; must be '0' to make 100
    jne not_100
    mov bl, 100
    jmp score_done
    
not_100:
    ; Not 100, so we have invalid score (>100)
    mov dl, 0dh     ; carriage return
    mov ah, 02h
    int 21h
    mov dl, ' '     ; space
    int 21h
    int 21h
    jmp read_first_digit
    
check_score_range:
    ; Check if <= 100
    cmp bl, 100
    jbe score_done
    
    ; Invalid score (>100)
    mov dl, 0dh     ; carriage return
    mov ah, 02h
    int 21h
    mov dl, ' '     ; space
    int 21h
    int 21h
    jmp read_first_digit
    
single_digit:
    mov al, bl      ; single digit is already correct
    jmp score_done_ax
    
score_done:
    mov al, bl
score_done_ax:
    xor ah, ah      ; clear upper byte
    pop dx
    pop cx
    pop bx
    ret
read_score endp

; 显示数字 (AX=number)
display_number proc near
    push ax
    push bx
    push cx
    push dx
    
    ; Check if number is zero
    test ax, ax
    jnz not_zero
    mov dl, '0'
    mov ah, 02h
    int 21h
    jmp display_done
    
not_zero:
    mov bx, 10
    xor cx, cx      ; counter for digits
    
divide_loop:
    xor dx, dx
    div bx          ; ax = quotient, dx = remainder
    push dx         ; save digit
    inc cx
    test ax, ax
    jnz divide_loop
    
print_loop:
    pop dx
    add dl, '0'
    mov ah, 02h
    int 21h
    loop print_loop
    
display_done:
    pop dx
    pop cx
    pop bx
    pop ax
    ret
display_number endp

;------------------------function2 查询学生信息-------------------------------------------------
check_stu proc near
    ; 检查是否有学生数据
    cmp [stu_count], 0
    jne has_students_fun2
    mov dx, offset no_students_msg
    mov ah, 09h
    int 21h
    ret

has_students_fun2:
    ; 显示查询菜单
    call show_query_menu
    ; 获取用户选择 (1:姓名, 2:ID)
    call get_query_choice
    ; 根据选择执行查询
    cmp al, '1'
    je search_by_name
    cmp al, '2'
    je search_by_id
    jmp invalid_choice

search_by_name:
    ; 提示输入姓名
    mov dx, offset input_name_prompt
    mov ah, 09h
    int 21h
    ; 读取姓名到search_buffer
    mov cx, 20
    lea di, search_buffer
    call read_string
    ; 设置查询类型标志
    mov bl, '1'
    jmp start_search

search_by_id:
    ; 提示输入ID
    mov dx, offset input_id_prompt
    mov ah, 09h
    int 21h
    ; 读取ID到search_buffer
    mov cx, 10
    lea di, search_buffer
    call read_string
    ; 设置查询类型标志
    mov bl, '2'
    jmp start_search

invalid_choice:
    ; 输入非法，提示重新输入
    mov dx, offset invalid_choice_msg
    mov ah, 09h
    int 21h
    jmp has_students_fun2

start_search:
    ; 初始化搜索
    mov cl, [stu_count]
    mov ch, 0
    lea si, students_array  ; SI指向学生数组

search_loop:
    push cx
    push si
    ; 根据查询类型决定比较哪个字段
    cmp bl, '1'
    je compare_name
    ; 比较ID字段
    lea di, [si].student_id
    jmp do_compare

compare_name:
    ; 比较姓名字段
    lea di, [si].student_name

do_compare:
    ; 比较输入和当前学生字段
    lea si, search_buffer
    call compare_strings
    jc found_student
    ; 不匹配，继续下一个学生
    pop si
    pop cx
    add si, size student_struct
    loop search_loop
    ; 没找到任何匹配
    mov dx, offset not_found_msg
    mov ah, 09h
    int 21h
    ret

found_student:
    ; 恢复学生结构指针
    pop si
    pop cx
    ; 显示学生信息
    call display_student_info
    ret
check_stu endp

; ===== 子程序 =====
; 显示查询菜单
show_query_menu proc near
    mov dx, offset check_menu_msg
    mov ah, 09h
    int 21h
    ret
show_query_menu endp

; 获取查询选择 (1或2)
get_query_choice proc near
    mov ah, 01h
    int 21h
    ret
get_query_choice endp

; 比较字符串 (SI=输入, DI=学生字段)
compare_strings proc near
    push si
    push di
compare_loop:
    mov al, [si]
    cmp al, '$'
    je check_end
    cmp al, [di]
    jne not_equal
    inc si
    inc di
    jmp compare_loop
check_end:
    cmp byte ptr [di], '$'
    je equal
not_equal:
    clc  ; 清除CF表示不匹配
    jmp compare_done
equal:
    stc  ; 设置CF表示匹配
compare_done:
    pop di
    pop si
    ret
compare_strings endp

; 显示学生信息 (SI=学生结构指针)
display_student_info proc near
    push si
    mov dx, offset student_info_header
    mov ah, 09h
    int 21h
    
    ; 显示姓名
    mov dx, offset name_display
    int 21h
    lea dx, [si].student_name
    int 21h
    
    ; 显示ID
    mov dx, offset id_display
    int 21h
    lea dx, [si].student_id
    int 21h
    
    ; 显示平时成绩
    mov dx, offset usual_display
    int 21h
    mov ax, [si].usual_score
    call display_number
    
    ; 显示大作业成绩
    mov dx, offset project_display
    mov ah, 09h
    int 21h
    mov ax, [si].project_score
    call display_number
    
    ; 显示最终成绩
    mov dx, offset final_display
    mov ah, 09h
    int 21h
    mov ax, [si].final_score
    call display_number
    
    ; 换行
    mov dx, offset newline
    mov ah, 09h
    int 21h
    pop si
    ret
display_student_info endp

;------------------------------function3 成绩排序-------------------------------
score_rank proc near
    push ax
    push bx
    push cx
    push dx
    push si
    push di
		
    ; 检查是否有学生数据
    cmp [stu_count], 0
    jne has_students_fun3
    mov dx, offset no_students_msg
    mov ah, 09h
    int 21h
    jmp sort_exit
    
has_students_fun3:
    ; 显示排序表头
    mov dx, offset sort_display_header
    mov ah, 09h
    int 21h
    mov dx, offset sort_display_line
    mov ah, 09h
    int 21h
    
    ; 初始化排序 (冒泡排序)
    mov cl, [stu_count]
    mov ch, 0  ; Now cx has the correct value
    dec cx          ; 外循环次数 = 学生数-1
    jz sort_done    ; 如果只有1个学生，不需要排序
    
outer_loop:
    push cx
    lea si, students_array
    mov di, si
    add di, size student_struct  ; DI指向下一个学生
    
inner_loop:
    ; 比较两个学生的总成绩
    mov ax, [si].final_score
    cmp ax, [di].final_score
    jge no_swap  ; Changed from jle to jge for descending order
    
    ; 交换两个学生记录
    push cx
    mov cx, size student_struct
swap_loop:
    mov al, [si]
    xchg al, [di]
    mov [si], al
    inc si
    inc di
    loop swap_loop
    pop cx
    
no_swap:
    ; 移动到下一个学生
    add si, size student_struct
    add di, size student_struct
    loop inner_loop
    
    pop cx
    loop outer_loop
    
sort_done:
    ; 显示排序结果
    mov [rank_num], 1       ; 初始化排名
    lea si, students_array
    mov cl, [stu_count]
    mov ch, 0  ; Now cx has the correct value
    
display_loop:
    push cx
    
    ; 显示排名
    mov dx, offset rank_display
    mov ah, 09h
    int 21h
    mov al, [rank_num]
    xor ah, ah
    call display_number
    mov dx, offset space_char
    mov ah, 09h
    int 21h
    
    ; 显示姓名
    lea dx, [si].student_name
    mov ah, 09h
    int 21h
    mov dx, offset space_char
    mov ah, 09h
    int 21h
    
    ; 显示学号
    lea dx, [si].student_id
    mov ah, 09h
    int 21h
    mov dx, offset space_char
    mov ah, 09h
    int 21h
    
    ; 显示平时成绩
    mov ax, [si].usual_score
    call display_number
    mov dx, offset space_char
    mov ah, 09h
    int 21h
    
    ; 显示大作业成绩
    mov ax, [si].project_score
    call display_number
    mov dx, offset space_char
    mov ah, 09h
    int 21h
    
    ; 显示总成绩
    mov ax, [si].final_score
    call display_number
    
    ; 换行
    mov dx, offset newline
    mov ah, 09h
    int 21h
    
    ; 更新到下一个学生
    add si, size student_struct
    inc [rank_num]      ; 排名递增
    
    pop cx
    loop display_loop
    
sort_exit:
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
score_rank endp

;------------------------function4 统计学生成绩-------------------------------------------------
grade_stats proc near
    ; 检查是否有学生数据
    cmp [stu_count], 0
    jne has_students_to_stat
    mov dx, offset no_students_msg
    mov ah, 09h
    int 21h
    ret

has_students_to_stat:
    ; 初始化统计计数器
    mov [score_seg90_100], 0
    mov [score_seg80_89], 0
    mov [score_seg60_79], 0
    mov [score_seg60_bel], 0
    mov [total_score], 0
    mov [total_score+2], 0  ; 高位清零
    mov [highest_score], 0
    mov [lowest_score], 100
    
    ; 遍历所有学生进行统计
    mov cl, [stu_count]
    mov ch, 0
    lea si, students_array
    
stat_loop:
    ; 获取当前学生的最终成绩
    mov ax, [si].final_score
    
    ; 更新总分(32位加法)
    add word ptr [total_score], ax
    adc word ptr [total_score+2], 0
    
    ; 更新最高分
    cmp ax, [highest_score]
    jbe not_higher
    mov [highest_score], ax
not_higher:
    
    ; 更新最低分
    cmp ax, [lowest_score]
    jae not_lower
    mov [lowest_score], ax
not_lower:
    
    ; 分数段统计
    cmp ax, 90
    jb below_90
    inc [score_seg90_100]
    jmp next_student
    
below_90:
    cmp ax, 80
    jb below_80
    inc [score_seg80_89]
    jmp next_student
    
below_80:
    cmp ax, 60
    jb below_60
    inc [score_seg60_79]
    jmp next_student
    
below_60:
    inc [score_seg60_bel]
    
next_student:
    add si, size student_struct
    loop stat_loop
    
    ; 计算平均分(32位除法)
    mov ax, word ptr [total_score]
    mov dx, word ptr [total_score+2]
    mov cl, [stu_count]  ; 将字节加载到cl（cx的低8位）
	mov ch, 0            ; 将cx的高8位清零
    div cx          ; ax = 商, dx = 余数
    mov [average_score], ax
    
    ; 显示统计结果
    call display_statistics
    ret
grade_stats endp

display_statistics proc near
    push ax
    push dx
    
    ; 显示统计标题
    mov dx, offset statistics_header
    mov ah, 09h
    int 21h
    
    ; 显示90-100分数段
    mov dx, offset seg90_100
    int 21h
    mov al, [score_seg90_100]
    xor ah, ah
    call display_number
    
    ; 显示80-89分数段
    mov dx, offset seg80_89
    mov ah, 09h
    int 21h
    mov al, [score_seg80_89]
    xor ah, ah
    call display_number
    
    ; 显示60-79分数段
    mov dx, offset seg60_79
    mov ah, 09h
    int 21h
    mov al, [score_seg60_79]
    xor ah, ah
    call display_number
    
    ; 显示60分以下分数段
    mov dx, offset seg_below60
    mov ah, 09h
    int 21h
    mov al, [score_seg60_bel]
    xor ah, ah
    call display_number
    
    ; 显示平均分
    mov dx, offset avg_score
    mov ah, 09h
    int 21h
    mov ax, [average_score]
    call display_number
    
    ; 显示最高分
    mov dx, offset max_score
    mov ah, 09h
    int 21h
    mov ax, [highest_score]
    call display_number
    
    ; 显示最低分
    mov dx, offset min_score
    mov ah, 09h
    int 21h
    mov ax, [lowest_score]
    call display_number
    
    ; 换行
    mov dx, offset newline
    mov ah, 09h
    int 21h
    
    pop dx
    pop ax
    ret
display_statistics endp



;------------------------------function5退出系统-------------------------------------------
exit_program proc near
    mov dx, offset goodbye
    mov ah, 09h
    int 21h
    mov ax,4c00h
    int 21h
exit_program endp

code ends
end start