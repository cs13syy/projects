# 1. 스토리지 버킷 생성
# 2. 데이터랩 생성 (클라우드쉘)
- 터미널에 명령어 입력 datalab create --machine-type n1-standard-8 datalab-instance
- zone은 us-east-1
- 참고: https://zzsza.github.io/gcp/2018/08/02/bigquery-and-datalab/
# 3. API 설정
- 구글클라우드 콘솔에서 API 및 서비스 사용 설정 
- Google Compute Engine, Cloud Source Repositories APIs 2개 사용 설정
# 4. 할당량 수정
- In-use address (8->10)
- CPUS (24->240)
- CPUS_ALL_REGIONS (32->100)
- 메일 오는 것 반드시 확인, 할당량 증가 실패하면 클러스터 생성 불가
# 5. DATA PROC 생성 & 클러스터 
- 마스터 노드(1개, 4vCPUs), 워커 노드(8개, 8vCPUs)의 성능과 개수 선택
- 초기화 작업 창에 다음 명령어 입력
- gs://dataproc-initialization-actions/datalab/datalab.sh
# 6. 클라우드 쉘에 접속 코드 입력
gcloud compute ssh cluster-1206-m \
    --project=zeta-infusion-224701 --zone us-east1-b -- \
    -4 -N -L 8080:cluster-1206-m:8080
