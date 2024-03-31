# 디렉토리 A의 경로 설정
$directoryA = "D:\work\lecture\Lecture_EOPL_hw\\HW1_Haskell-mooc_exercise"

# 디렉토리 A 내의 모든 Set9a.hs 파일 찾기
$files = Get-ChildItem -Path $directoryA -Filter Set9a.hs -Recurse

# 모든 파일 조합에 대해 fc 명령어 실행
for ($i = 0; $i -lt $files.Count; $i++) {
    for ($j = $i + 1; $j -lt $files.Count; $j++) {
        $file1 = $files[$i].FullName
        $file2 = $files[$j].FullName

        # 파일 비교 실행
        Write-Output "Compare $file1 "
        Write-Output "       with $file2 "
        $fcOutput = cmd /c fc $file1 $file2

       # 차이가 있는 라인 수 계산
       $fcOutput | Measure-Object -Line
    }
}
