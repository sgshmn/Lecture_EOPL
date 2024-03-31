# 지정한 Set9a.hs 파일과 디렉토리 A의 경로
$targetFile = ".\Set9a.hs"
$directoryA = "D:\work\lecture\Lecture_EOPL_hw\HW1_Haskell-mooc_exercise"

# 지정한 파일의 내용을 읽기
$targetContent = Get-Content -Path $targetFile

# 디렉토리 A 내의 모든 Set9a.hs 파일 찾기
$files = Get-ChildItem -Path $directoryA -Filter Set9a.hs -Recurse

foreach ($file in $files) {
    # 각 파일의 경로 출력
    # Write-Output "파일 경로: $($file.FullName)"

    # 현재 파일의 내용을 읽기
    $currentContent = Get-Content -Path $file.FullName
    
    # 내용 비교
    if (Compare-Object -ReferenceObject $targetContent -DifferenceObject $currentContent -SyncWindow 0) {
        # 내용이 다르면 아무 것도 하지 않음
    } else {
        # 내용이 같으면 경로 출력
        Write-Output "$($file.FullName)"
    }
}
