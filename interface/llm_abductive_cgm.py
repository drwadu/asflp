import requests

url = "https://api-inference.huggingface.co/models/meta-llama/Meta-Llama-3-8B-Instruct"
token = open("token.txt", 'r').read().strip()  # Replace with your Hugging Face token

def llm(query):
  parameters = {
      "max_new_tokens": 5000,
      "temperature": 0.01,
      "top_k": 50,
      "top_p": 0.95,
      "return_full_text": False
      }
  

  # NOTE: tries to write logic program
  # NOTE: ultimately we try to parse inputs
  # for now it's more or less a playground
  prompt = """<|begin_of_text|><|start_header_id|>system<|end_header_id|>You are a helpful and smart assistant. A CGM user will ask you for help to determine why their CGM has fallen off. You accurately provide an answer to the provided user query. Based on your answer write a logic program that encodes reasons for the CGM falling off by means of rules, and the specific user information that you can retrieve from the query.<|eot_id|><|start_header_id|>user<|end_header_id|> Here is the query: ```{query}```.
      Provide precise and concise answer.<|eot_id|><|start_header_id|>assistant<|end_header_id|>"""
  
  headers = {
      'Authorization': f'Bearer {token}',
      'Content-Type': 'application/json'
  }
  
  prompt = prompt.replace("{query}", query)
  
  payload = {
      "inputs": prompt,
      "parameters": parameters
  }
  
  response = requests.post(url, headers=headers, json=payload)
  response_text = response.json()[0]['generated_text'].strip()

  return response_text

print(llm("My CGM has fallen off after only one day of wearing it. What could be the reason?"))
