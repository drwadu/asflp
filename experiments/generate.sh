for n in $(seq 1 1000)
do
	python3 sampler.py > h1_322/p$n.flp
done
