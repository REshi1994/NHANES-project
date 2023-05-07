import csv
from shutil import rmtree
from pandas import read_csv, read_sas, DataFrame, read_fwf
from re import findall, search
from os.path import exists, getsize
from os import makedirs, listdir, rename
from concurrent.futures import ThreadPoolExecutor
from requests import get, adapters, head
from lxml import etree
from numpy import nan
from time import sleep, time, localtime, strftime
import pdfplumber
from queue import  Queue
from threading import Thread

class nhanes_d():


    dir = "E:/dataset/NHANES"
    host = "https://wwwn.cdc.gov"
    headers = {
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:101.0) Gecko/20100101 Firefox/101.0"
    }
    headers2 = {
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:101.0) Gecko/20100101 Firefox/101.0"
    }
    urlmode = "https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx"
    mort_linkage = 'https://www.cdc.gov/nchs/data-linkage/mortality-public.htm'
    mort_ftp = 'https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/'
    mort_help_xpath = '/html/body/div[2]/main/div[3]/div/div[3]/ul'
    host_mort = 'https://www.cdc.gov'
    host_mort_ftp = 'https://ftp.cdc.gov'
    mort_data_xpath = '/html/body/pre/a'
    mort_ftp_body = None

    def __init__(self):
        self.setdir()
        if not exists(self.dir):
            makedirs(self.dir)
        # super().__init__()
    def setdir(self):
        now = strftime('%y%m%d', localtime(time()))
        self.dir = self.dir + now

    def response_table(self):
        argus =  { 'Component': self.comp_dealed, 'Cycle': self.cyl_dealed}
        try:
            response = get(url=self.urlmode, headers=self.headers, params=argus).text
        except:
            response = get(url=self.urlmode, headers=self.headers2, params=argus).text
        return response
    def tree(self):
        tree = etree.HTML(self.response_table())
        return tree
    def divthead(self):
        thead = '//table/thead//th/text()'
        return thead
    def extraThead(self):
        thead = self.tree().xpath(self.divthead())
        return thead
    def deal_comps(self, components=None):
        component = ['Demographics', 'Questionnaire', 'Laboratory', 'Examination', 'Dietary','Mortality']
        components = None if components is None \
            else [components] if (type(components) == str and components) or type(components) == int \
            else [i for i in components if i != '']
        find_comp = []
        if components is None or components == []:
            find_comp = component
        else:
            for item in components:
                if type(item) == int:
                    find_comp.append(item)
                else:
                    for i in component:
                        find_str = i if i.lower().find(item.lower()) != -1 else ""
                        if find_str:
                            index_int = component.index(i)
                            find_comp.append((item, find_str, index_int))
            find_comp = [component[comp] if type(comp) == int else comp[-2] for comp in find_comp]
            find_comp = sorted(set(find_comp), key=find_comp.index)

        return find_comp
    def deal_cycles(self,cycles=None):
        years = [cycles] if type(cycles) == int else cycles if cycles is not None else [i for i in range(1999, 2018)]
        cycle1 = []
        for i in years:
            cycle1.append(str(i) + "-" + str(i + 1) if i % 2 else str(i - 1) + "-" + str(i))

        cycle1 = sorted(list(set(cycle1)), key=cycle1.index)
        return cycle1
    def set_self_cyl(self, eval):
        self.cyl_dealed = eval
    def set_self_comp(self, eval):
        self.comp_dealed = eval
    def set_self_dir(self, eval):
        self.dir = eval
    def current_date(self):
        now = strftime('%y-%m-%d %H:%M:%S', localtime(time()))
        return now
    def get_mort_tree(self, url):
        try:
            response = get(url=url, headers=self.headers).text
        except:
            response = get(url=url, headers=self.headers2).text
        tree = etree.HTML(response)
        return tree
    def replace_rnt(self, rnt_text):
        text_new = rnt_text.replace("\r", "").replace("\n", "").replace("\t", "")
        return text_new
    def extra_mort_help(self):
        tree = self.get_mort_tree(self.mort_linkage)
        ul = tree.xpath(self.mort_help_xpath)

        for ul in ul:
            lis = ul.xpath('./li')
            help_file_list = [['File Name', 'File Web']]
            for li in lis:
                file_name = [self.replace_rnt(i) for i in li.xpath('./text()')]
                file_name = [i for i in file_name if i]
                if file_name:
                    file_name = [i.replace(''.join(findall(':.*$', i)), '') if findall(':.*$', i) else i   for i in file_name]
                    if self.findkey(['file desc', 'data dict'], ''.join(file_name)):
                        href = [self.replace_rnt(i) for i in li.xpath('./a/@href')]
                        href = [i for i in href if i]
                        if href:
                            href = [f"{self.host_mort}{i}" if self.findkey(['^/nchs.+pdf'], i) else i for i in href ]
                            row = file_name + href
                            help_file_list.append(row)
                    elif self.findkey(['software program'], ''.join(file_name)):
                        li2s = li.xpath('./ul/li')
                        for li2 in li2s:
                            href = [self.replace_rnt(i) for i in li2.xpath('./a/@href')]
                            href = [i for i in href if i]
                            if href:
                                row = file_name + href
                                help_file_list.append(row)

        return help_file_list

    def extra_mort_body(self, cyl_deald=None, data_src='nhanes'):
        # help_list = self.extra_mort_help()
        if self.mort_ftp_body is None:
            cyl_deald2 = cyl_deald.replace('-', "_")
            key = f"{data_src}.+{cyl_deald2}"
            tree = self.get_mort_tree(self.mort_ftp)
            a_all = tree.xpath(self.mort_data_xpath)
        table = []
        title = [['Cycle', 'File Name', 'File Web']]

        for a in a_all:
            href = [self.replace_rnt(i) for i in a.xpath('./@href')]
            href = [i for i in href if i]
            if not href:
                href = ['']
            else:
                href = [f"{self.host_mort_ftp}{i}" for i in href if self.findkey(['^/pub.+.dat'], i)]

            file_name = [self.replace_rnt(i) for i in a.xpath('./text()')]
            file_name = [i for i in file_name if i]
            if file_name:
                if self.findkey([key], ''.join(file_name)):
                    row = file_name + href
                    table.append(row)
        # table = table + help_list
        table = [[cyl_deald] + i for i in table]
        table = title + table
        return table

    def findkey(self, patterns, string, lower=True, mode='|', return_value=False):

        if lower:
            patterns2 = [ f'{i}'.lower() for i in patterns]
            string2 = f'{string}'.lower()
        else:
            patterns2 = [ f'{i}' for i in patterns]
            string2 = f'{string}'
        if mode == '|':
            value = False if not return_value else ''
            for pattern in patterns2:

                find_str = search(pattern=pattern, string=string2)
                if find_str is not None:
                    if return_value:
                        span = find_str.span()
                        value = string[span[0]:span[1]]
                    else:
                        value = True
                    break

        if mode == '&':
            value = False if not return_value else ''
            find_num = 0
            for pattern in patterns2:
                find_str = search(pattern=pattern, string=string2)
                if find_str is not None:
                    find_num += 1
            if find_num == len(patterns2):
                value = string if return_value else True


        return value

    def extra_body(self, comp_dealed=None,cyl_dealed=None):

        if comp_dealed is not None:
            self.comp_dealed=comp_dealed
        if cyl_dealed is not None:
            self.cyl_dealed=cyl_dealed
        tbody_text = '//tbody/tr'
        tbody = self.tree().xpath(tbody_text)
        title = ['Cycle'] + self.extraThead() + ['Doc Web', 'XPT Web']
        title = [[ ' '.join(findall('[a-zA-Z]+', i))  for i in title]]
        # print(title)
        table = []

        for tr in tbody:
            tds = tr.xpath('./td')
            row_text = []
            href_list = []
            for td in tds:
                td_text = [self.replace_rnt(i) for i in td.xpath('./text()')]
                td_text = [i for i in td_text if i]
                if not td_text:
                    td_text = [self.replace_rnt(i) for i in td.xpath('./a/text()')]
                    td_text = [i for i in td_text if i]

                    href = [self.replace_rnt(i) for i in td.xpath("./a/@href")]
                    href = [i for i in href if i]
                    if not href:
                        href = ['']
                    elif href and self.findkey(['^/Nchs.+\.xpt', '^/Nchs.+\.htm'], href[0]):
                        href = [f"{self.host}{i}" for i in href]
                    href_list = href_list + href

                if not td_text:
                    td_text = ['']


                row_text = row_text + td_text
            row_text = row_text + href_list
            table.append(row_text)
        table = [[cyl_dealed] + i for i in table]
        table = title + table
        return table

    def save_table(self, table, dst):
        with open(dst, mode='w', encoding='utf-8', newline='') as fp:
            table_file = csv.writer(fp)
            table_file.writerows(table)

    def update_tables(self, comps=None, cyls=None):
        comps = self.deal_comps(comps)
        cyls = self.deal_cycles(cyls)

        for comp in comps:
            print(f"获取 {comp} tables：", end='\t')
            with ThreadPoolExecutor(max_workers=len(cyls))as poolT:
                if not self.findkey(['mort'], comp):
                    reqs = [poolT.submit(self.extra_body, comp, cyl) for cyl in cyls]
                else:
                    reqs = [poolT.submit(self.extra_mort_body, cyl) for cyl in cyls]
                for req, cyl in zip(reqs, cyls):
                    table = req.result()
                    save_to = f"{self.dir}/{comp}/{cyl}"
                    dst = f"{save_to}/{comp} {cyl}  items {len(table) -1}.update_table"
                    if not exists(save_to):
                        makedirs(save_to)
                    self.save_table(table, dst)
                    print(f"{cyl} OK", end=";\t")
            print('\n')
            if self.findkey(['mort'], comp):
                help_list = self.extra_mort_help()
                save_to = f"{self.dir}/{comp}"
                dst = f"{save_to}/{comp}  items {len(help_list) -1}.help_files"
                if not exists(dst):
                    print(f"获取 {comp} Help Files:", end='\t')
                    with open(dst, mode='w', encoding='utf-8', newline='')as fp:
                        for i in help_list:
                            csv.writer(fp).writerow(i)
                            web = i[-1]
                            filename = self.findkey(['file.*-.*descrip.+.pdf', 'file.*-.*dict.+.pdf', 'sas_.+surveys.sas', 'stata_.+surveys.do', 'R_.+surveys.R'], web, return_value=True)
                            if filename:
                                file_dst = f"{save_to}/{filename}"
                                self.download_file(web, file_dst)
                                print(f"{filename} OK;", end='\t')
                else:
                    help_file = self.read_csv(dst)
                    webs = help_file['File Web'].values
                    for web in webs:
                        filename = self.findkey(['file.*-.*descrip.+.pdf', 'file.*-.*dict.+.pdf', 'sas_.+surveys.sas', 'stata_.+surveys.do', 'R_.+surveys.R'], web, return_value=True)
                        if filename:
                            file_dst = f"{save_to}/{filename}"
                            if not exists(file_dst):
                                self.download_file(web, file_dst)
                                print(f"downloading {filename} OK;", end='\t')
                    print('\n')

    def single_thread_download(self, web, dst):

        try:
            req = get(web,headers=self.headers,stream=True)
        except:
            sleep(3)
            req = get(web, headers=self.headers2,stream=True)
        with open(dst, mode='wb') as fp:
            for chunk in req.iter_content(chunk_size=1024):
                if chunk:
                    fp.write(chunk)
    def download_file(self, web, dst, Data_File=None, start_mtd=5):
        filename = self.findkey(['[^/]+$'], dst, return_value=True)

        if Data_File is None:
            self.single_thread_download(web, dst)
        else:
            size_str = self.findkey(['\[.+\]'], Data_File, return_value=True)
            size_MB = self.findkey(['MB'], size_str, return_value=True)
            size_num = self.findkey(['[0-9.]+'], size_str, return_value=True)
            size_GB = self.findkey(['GB'], size_str, return_value=True)
            if size_GB:
                print(f"{filename} Failed")
            elif size_MB and float(size_num) > start_mtd:
                MTD().run(web, dst)
            else:
                # self.plock.acquire()
                self.single_thread_download(web, dst)
                # self.plock.release()

        return filename

    def deal_table(self, dst):
        df = self.read_csv(dst)
        if findall('Mortality.+', dst):
            log_formu = df['File Web'].apply(lambda x: True if self.findkey(['https://ftp.+.dat'], x) else False)
            df2 = df[log_formu]
            if df2.empty:
                df2 = None
            return [df2, None]


        else:
            log_formu = ((df['Doc Web'].apply(lambda x: True if self.findkey(['https://.+.htm'], f"{x}") else False)) &
                         (df['XPT Web'].apply(lambda x: True if self.findkey(['https://.+.xpt'], f"{x}") else False))  )
            df2 = df[log_formu]
            if df2.empty:
                df2 = None

            df_error = df[~log_formu]
            if df_error.empty:
                df_error = None

            return [df2, df_error]
    def deal_tables(self, comps=None, cyls=None):
        comps = self.deal_comps(comps)
        cyls = self.deal_cycles(cyls)
        for comp in comps:
            print(f"处理 {comp} tables：", end='\t')
            for cyl in cyls:
                path = f"{self.dir}/{comp}/{cyl}"
                files = listdir(path)
                file = [i for i in files if findall(f"{comp} +{cyl} +items +[0-9]+", i)]
                if file:
                    file = file[0]
                    dst = f"{self.dir}/{comp}/{cyl}/{file}"

                    if exists(dst):
                        df = self.deal_table(dst)
                        if df[1] is not None:
                            df_error = df[1]
                            df_error_dst = f"{path}/error items {len(df_error.index)}.df_error"
                            df_error.to_csv(df_error_dst, index=False)
                            print(f"{cyl} df_error", end='\t')
                        if df[0] is not None:
                            df_normal = df[0]
                            df_normal_dst = f"{path}/normal items {len(df_normal.index)}.df_normal"
                            df_normal.to_csv(df_normal_dst, index=False)
                            print(f"{cyl} OK", end=";\t")
                        else:
                            print(f"{cyl} Failed", end=";\t")


            print('\n')

    def deal_lab_error(self, df_error_dst):

        path = df_error_dst.replace('.df_error', '')
        if not exists(path):
            makedirs(path)
        df = self.read_csv(df_error_dst)


        df_vid = df[df['Doc Web'].apply(lambda x: self.findkey(['vid[_A-Za-z0-9]*.*=htm'], x))]
        df_vid.index = range(len(df_vid.index))
        if not df_vid.empty:
            for i in df_vid.index:
                cyl = df_vid.iloc[i, :]['Cycle']
                doc_file = df_vid.iloc[i, :]['Doc File']
                web = df_vid.iloc[i, :]['Doc Web']
            # for cyl, doc_file, web in zip(df_vid['Cycle'].values, df_vid['Doc File'].values, df_vid['Doc Web'].values):
                table = []
                doc_name = self.findkey(['vid[_A-Za-z0-9]*'], doc_file, return_value=True)
                print(f"更新 {cyl} {doc_name}:", end='\t')
                host2 = f"{self.host}/nchs/nhanes"
                web = f"{host2}/{web.replace('..', '')}"
                tree = self.get_mort_tree(web)
                html_xpath = '/html/body/form/div[8]/main/div[4]/div/div[3]/div/div/span/a'
                href_htm = [self.replace_rnt(i) for i in tree.xpath(f"{html_xpath}/@href")]
                href_htm = [i for i in href_htm if i]
                if href_htm:
                    href_htm = "".join([f"{host2}/{i.replace('..', '')}" for i in href_htm])
                    href_xpt = href_htm.replace('.htm', '.xpt')
                    # print(href_htm, href_xpt)
                else:
                    href_htm = ['']
                    href_xpt = ['']
                # row = [cyl, doc_file, href_htm, href_xpt]
                row = list(df_vid.iloc[i, :-2]) + [href_htm, href_xpt]
                table.append(row)
                items = len(table)
                # title = ['Cycle', 'Doc File', 'Doc Web', 'XPT Web']
                title = list(df_vid.columns)
                table = [title] + table
                path_to = f"{path}/{doc_name}"
                if not exists(path_to):
                    makedirs(path_to)
                error_dst = f"{path_to}/{doc_name}  items {items}.update_table"
                with open(error_dst, mode='w', encoding='utf-8', newline='') as fp:
                    df_error = csv.writer(fp)
                    df_error.writerows(table)

                print(f"{doc_name} OK;")

        df_oral = df[df['XPT Web'].apply(lambda x: self.findkey(['/nchs/nhanes/omp.+.aspx'], x))]
        df_oral.index = range(len(df_oral.index))
        if not df_oral.empty:
            for i in df_oral.index:
                cyl = df_oral.iloc[i, :]['Cycle']
                doc_name = df_oral.iloc[i, :]['Data File Name']
                web = df_oral.iloc[i, :]['XPT Web']
            # for cyl,data_name, web in zip(df_oral['Cycle'].values, df_oral['Data File Name'].values, df_oral['XPT Web'].values):
            #     doc_name = data_name
                table = []
                web = self.findkey(['/nchs/nhanes/omp.+.aspx'], web, return_value=True)
                if web:
                    web = f"{self.host}{web}"
                    print(f"更新 {cyl} OMP:", end='\t')
                    tree = self.get_mort_tree(web)
                    dxa_xpath = '/html/body/form/div[8]/main/div[4]/div/div[3]/div/div/table/tbody/tr'
                    tbody = tree.xpath(dxa_xpath)
                    for tr in tbody:

                        tds = tr.xpath('./td')
                        row_text = []
                        href_list = []
                        for inde, td in enumerate(tds):

                            td_text = [self.replace_rnt(i) for i in td.xpath('./text()')]
                            td_text = [i for i in td_text if i]

                            if not td_text:
                                td_text = [self.replace_rnt(i) for i in td.xpath('./a/text()')]
                                td_text = [i for i in td_text if i]


                                href = [self.replace_rnt(i) for i in td.xpath("./a/@href")]
                                href = [i for i in href if i]
                                if not href:
                                    href = ['']
                                elif href and self.findkey(['/Nchs.+\.txt'], href[0]):
                                    href = [f"{self.host}{i}" for i in href]
                                href = [(inde, i) for i in href]
                                href_list = href_list + href
                            if not td_text:
                                td_text = ['']

                            row_text = row_text + td_text
                        row_text = row_text[:2] + [''] + row_text[2:] if len(row_text) ==3 else row_text

                        href_list = ['', ''] if not href_list else [i[1] for i in href_list] if len(href_list) ==2 and href_list else [href_list[0][1], ''] if len(href_list) ==1 and href_list[0][0] == 1 else ['', href_list[0][1]]


                        row_text = row_text + href_list
                        table.append(row_text)
                        dropstr = self.findkey(['\[.+\]'], row_text[1], return_value=True)
                        filename = row_text[1].replace(dropstr, '')
                        print(f"{filename} OK", end=';\t')
                    print('\n')

                table = [[cyl] + i  for i in table]
                items = len(table)
                title = ['Cycle', 'Data File Name', 'Data File', 'Variable/Annotation File', 'Date Published', 'File Web', 'Variable/Annotation Web']
                table = [title] + table
                path_to = f"{path}/{doc_name}"
                if not exists(path_to):
                    makedirs(path_to)
                error_dst = f"{path_to}/{doc_name}  items {items}.update_table"
                with open(error_dst, mode='w', encoding='utf-8', newline='') as fp:
                    df_error = csv.writer(fp)
                    df_error.writerows(table)

        df_withdrawn = df[df['Date Published'].apply(lambda x: self.findkey(['withdrawn'], x))]
        df_withdrawn.index = range(len(df_withdrawn.index))
        if not df_withdrawn.empty:
            for i in df_withdrawn.index:
                cyl = df_withdrawn.iloc[i, :]['Cycle']
                doc_name = df_withdrawn.iloc[i, :]['Doc File']
                doc_name = self.findkey(['[A-Za-z0-9_]+'], doc_name, return_value=True)
                path_to = f"{path}/{doc_name} Withdrawn"
                if not exists(path_to):
                    makedirs(path_to)
                print(f"更新 {cyl} {doc_name}:", end='\t')
                print('Withdrawn', end=';\n')
        # return df
    def deal_exam_error(self, df_error_dst):
        path = df_error_dst.replace('.df_error', '')
        if not exists(path):
            makedirs(path)
        df = self.read_csv(df_error_dst)

        df_pax = df[df['XPT Web'].apply(lambda x: self.findkey(['pax[_A-Za-z0-9]*', 'ftp'], x, mode='&'))]
        df_pax.index = range(len(df_pax.index))
        if not df_pax.empty:
            for i in df_pax.index:
                cyl = df_pax.iloc[i, :]['Cycle']
                data_name = df_pax.iloc[i, :]['Data File']
                web = df_pax.iloc[i, :]['XPT Web']
                prefix = df_pax.iloc[i, :]
                doc_name = self.findkey(['pax[A-Za-z0-9_]*'], data_name, return_value=True)
                print(f"更新 {cyl} {doc_name}:", end='\t')
                tree = self.get_mort_tree(web)
                paxfile_xpath = '/html/body/pre/a'
                aaas = tree.xpath(paxfile_xpath)
                table = []
                for a in aaas:
                    filename = [self.findkey(['[0-9]+.+tar.bz2'], i, return_value=True) for i in a.xpath('./text()') if self.findkey(['[0-9]+.+tar.bz2'], i)]
                    filename = [i for i in filename if i]
                    if not filename:
                        filename = ['']

                    href = [self.findkey(['/pub/pax.+tar.bz2'], i, return_value=True) for i in a.xpath('./@href') if self.findkey(['/pub/pax.+tar.bz2'], i)]
                    href = [i for i in href]
                    if not href:
                        href = ['']
                    else:
                        href = [f"{self.host_mort_ftp}{i}" for i in href]

                    row = filename + href
                    row = [i for i in row if i]
                    if len(row) > 1:
                        table.append(row)
                        name = ''.join(filename)
                        print(f"{name} OK", end=';\t')
                    else:
                        name = ''.join(filename)
                        if name:
                            print(f"{name} Failed", end=';\t')
                print('\n')
                items = len(table)
                table = [['']*len(prefix) + i if inde!=0 else list(prefix) + i for inde, i in enumerate(table)]
                title = [list(df_pax.columns) + ['File Name', 'File Web']]
                table = title + table
                path_to = f"{path}/{doc_name}"
                if not exists(path_to):
                    makedirs(path_to)
                error_dst = f"{path_to}/{doc_name}  items {items}.update_table"
                with open(error_dst, mode='w', encoding='utf-8', newline='') as fp:
                    df_error = csv.writer(fp)
                    df_error.writerows(table)



        df_zip = df[df['XPT Web'].apply(lambda x: self.findkey(['/nchs/nhanes.+.zip'], x))]
        df_zip.index = range(len(df_zip.index))
        if not df_zip.empty:
            for i in df_zip.index:
                cyl = df_zip.iloc[i, :]['Cycle']
                data_name = df_zip.iloc[i, :]['Data File']
                web = df_zip.iloc[i, :]['XPT Web']
                prefix = df_zip.iloc[i, :]

                table = []
                doc_name = self.findkey(['.*RAW[_A-Za-z0-9]*'], data_name, return_value=True)
                print(f"更新 {cyl} {doc_name}:", end='\t')
                web = self.findkey(['/nchs/nhanes.+.zip'] , web, return_value=True)
                if web:
                    web = self.host + web
                else:
                    web = ''
                if doc_name and web:
                    row = [doc_name , web]
                    table.append(row)
                    print(f"{doc_name} OK")
                else:
                    print(f"{doc_name} Failed")

                items = len(table)
                table = [['']*len(prefix) + i if inde!=0 else list(prefix) + i for inde, i in enumerate(table)]
                title = [list(df_zip.columns) + ['File Name', 'File Web']]
                table = title + table
                path_to = f"{path}/{doc_name}"
                if not exists(path_to):
                    makedirs(path_to)
                error_dst = f"{path_to}/{doc_name}  items {items}.update_table"
                with open(error_dst, mode='w', encoding='utf-8', newline='') as fp:
                    df_error = csv.writer(fp)
                    df_error.writerows(table)


        df_dxx = df[df['XPT Web'].apply(lambda x: self.findkey(['/nchs/nhanes/dxa.+.aspx'], x))]
        df_dxx.index = range(len(df_dxx.index))
        if not df_dxx.empty:
            for i in df_dxx.index:
                cyl = df_dxx.iloc[i, :]['Cycle']
                web = df_dxx.iloc[i, :]['XPT Web']
                doc_name = ''
                table = []
                if web:
                    web = f"{self.host}{web}"
                    print(f"更新 {cyl} DXA:", end='\t')
                    tree = self.get_mort_tree(web)
                    dxa_xpath = '/html/body/form/div[8]/main/div[4]/div/div[3]/div/div/table/tbody/tr'
                    tbody = tree.xpath(dxa_xpath)
                    for tr in tbody:
                        tr_texts = tr.xpath('.//text()')
                        match_cyl = [i for i in tr_texts if self.findkey([cyl], i)]
                        if not match_cyl:
                            continue
                        else:
                            print(f"{match_cyl}")
                            tds = tr.xpath('./td')
                            row_text = []
                            href_list = []
                            for td in tds:

                                td_text = [self.replace_rnt(i) for i in td.xpath('./text()')]
                                td_text = [i for i in td_text if i]

                                if not td_text:
                                    td_text = [self.replace_rnt(i) for i in td.xpath('./a/text()')]
                                    td_text = [i for i in td_text if i]
                                    doc_name = [self.findkey(['DXX[_A-Za-z0-9]*'], i, return_value=True) for i in td_text] if td_text else doc_name
                                    doc_name = ''.join(doc_name) if doc_name else doc_name

                                    href = [self.replace_rnt(i) for i in td.xpath("./a/@href")]
                                    href = [i for i in href if i]
                                    if not href:
                                        href = ['']
                                    elif href and self.findkey(['/Nchs.+\.xpt', '/Nchs.+\.htm', '/Nchs.+\.pdf'], href[0]):
                                        href = [f"{self.host}{i}" for i in href]
                                    href_list = href_list + href
                                if not td_text:
                                    td_text = ['']

                                row_text = row_text + td_text

                            row_text = row_text + href_list
                            table.append(row_text)
                            print(f"{doc_name} OK;")


                table = [i for i in table if [j for j in i if cyl == j]]
                items = len(table)
                title = ['Cycle', 'Data File Name', 'Doc File', 'Data File', 'Date Published', 'Doc Web', 'XPT Web']
                table = [title] + table
                path_to = f"{path}/{doc_name}"
                if not exists(path_to):
                    makedirs(path_to)
                error_dst = f"{path_to}/{doc_name}  items {items}.update_table"
                with open(error_dst, mode='w', encoding='utf-8', newline='') as fp:
                    df_error = csv.writer(fp)
                    df_error.writerows(table)

        df_withdrawn = df[df['Date Published'].apply(lambda x: self.findkey(['withdrawn'], x))]
        df_withdrawn.index = range(len(df_withdrawn.index))
        if not df_withdrawn.empty:
            for i in df_withdrawn.index:
                cyl = df_withdrawn.iloc[i, :]['Cycle']
                doc_name = df_withdrawn.iloc[i, :]['Doc File']
                doc_name = self.findkey(['[A-Za-z0-9_]+'], doc_name, return_value=True)
                path_to = f"{path}/{doc_name} Withdrawn"
                if not exists(path_to):
                    makedirs(path_to)
                print(f"更新 {cyl} {doc_name}:", end='\t')
                print('Withdrawn', end=';\n')
            # for cyl, doc_name in zip(df_withdrawn['Cycle'].values, df_withdrawn['Doc File'].values):
            #     doc_name = self.findkey(['[A-Za-z0-9_]+'], doc_name, return_value=True)
            #     print(f"更新 {cyl} {doc_name}:", end='\t')
            #     print('Withdrawn', end=';\n')
    def deal_errors(self, comps=None, cyls=None):
        comps = self.deal_comps(comps)
        cyls = self.deal_cycles(cyls)
        for comp in comps:
            print(f"处理 {comp} errors：")
            for cyl in cyls:
                path = f"{self.dir}/{comp}/{cyl}"
                files = listdir(path)
                file = [i for i in files if findall(f"error +items +[0-9]+", i)]
                if file:
                    file = file[0]
                    dst = f"{path}/{file}"

                    if exists(dst):
                        if self.findkey(['exam'], comp):
                            self.deal_exam_error(dst)
                        elif self.findkey(['lab'], comp):
                            self.deal_lab_error(dst)

    def web_special_str(self, comp):
        if self.findkey(['lab', 'exam', 'ques'], comp):
            special_str = [('Âµ', "u")]
        elif self.findkey(['diet'], comp):
            special_str = [('Ã', ""), ('Â', ''), ('\x82',''), ('®','')]

        return  special_str

    def deal_special_str(self, table_new, comp):
        special_str = self.web_special_str(comp)
        for spe_str in special_str:
            table_new = [[j.replace(spe_str[0], spe_str[1]) for j in i] for i in table_new]
        return table_new

    def get_codebook(self,comp, cyl, file_name, web, save_dst):
        # save_dst = f"{save_path}/{file_name}.codebook"
        codebook_xpath = '/html/body//div[@id="Codebook"]/div'
        tree = self.get_mort_tree(web)
        codebooks = tree.xpath(codebook_xpath)
        # print(codebooks)

        title1 = ['Component', 'Cycle', 'File Name', 'Variable Name', 'Sas Label', 'Target']
        title2 = ['Code or Value', 'Value Description', 'Count', 'Cumulative']
        title = title1 + title2
        table_list = [title]
        for var in codebooks:
            h3 = var.xpath('./h3/text()')
            if not self.findkey(['check'], h3):
                row_new = [comp, cyl, file_name]
                dts = var.xpath('./dl/dt')
                colnames = [self.replace_rnt("".join(i.xpath('./text()'))) for i in dts]
                dds = var.xpath('./dl/dd')
                labels = [self.replace_rnt("".join(i.xpath('./text()'))) for i in dds]

                target_inde = [inde for inde, i in enumerate(colnames) if self.findkey(['target'], i)]
                other_inde = [inde for inde, i in enumerate(colnames) if self.findkey(['variable name', 'sas label'], i)]
                # print(labels, target_inde, other_inde)
                target_labels = [labels[i] for i in target_inde]
                target_labels = '\n'.join(target_labels) if len(target_inde)>1 else ''.join(target_labels)
                other_labels = [labels[i] for i in other_inde]

                labels = other_labels + [target_labels]
                row_new = row_new + labels
                table = var.xpath('./table/tbody/tr')

                if table:
                    table_rows = [row.xpath('./td/text()')[:-1] for row in table]
                    table_rows = [[''.join(k) for k in i] for i in table_rows]
                    table_rows = [[self.replace_rnt(k) for k in i] for i in table_rows]
                    row_new = [row_new + i for i in table_rows]

                else:
                    row_new = [row_new + ['']*4]

            table_list = table_list + row_new
        return [table_list, save_dst]


    def get_mort_dict_dst(self):
        comps = listdir(self.dir)
        mort = [i for i in comps if self.findkey(['mort'], i)]
        if mort:
            mort = ''.join(mort)
            files = listdir(f"{self.dir}/{mort}")
            file_dict = [i for i in files if self.findkey(['file.+dict.+.pdf'], i)]
            if file_dict:
                file_dict = ''.join(file_dict)
                dst = f"{self.dir}/{mort}/{file_dict}"
                print(f"获取 {file_dict} 文件路径: {dst} OK;")
        else:
            print('Mort 文件夹不存在！')

        return dst
    def get_mort_dict_from_pdf(self):
        dst = self.get_mort_dict_dst()
        file_dict = self.findkey(['file.+dict.+.pdf'], dst, return_value=True)
        tables = []
        print(f"解析 Mortality Dictionary from {file_dict}:", end='\t')
        with pdfplumber.open(dst) as pdf:
            pages = pdf.pages
            pages = pages if len(pages)>1 else [pages]

            for inde, each in enumerate(pages):
                table = each.extract_table()
                title = table[:1]
                table = table[1:]
                tables = tables + table
                print(f"Page {inde + 1} OK;", end='\t')

            tables = title + tables
        print('\n', end='')
        tables = self.deal_mort_dict(tables)
        return tables
    def deal_mort_dict(self,codebook_mort):
        print(f"清理 Mortality Dictionary:", end='\t')

        codebook_mort = [[j.replace("\n", '') for j in i] for i in codebook_mort]
        print(f"replace %s to '' OK;" % r'\n', end='\t')

        df = DataFrame(codebook_mort[1:], columns=codebook_mort[0])
        df['Value Description'] = df['Value Description'].apply(lambda x: self.findkey(['[^ :][^.:：]+$'], x, return_value=True))
        print(f"drop {f'[^ :：][^.:：]+$'} from {'Value Description'} OK;")

        variable_names = [i for i in df['Variable Name'].values]
        variable_names2 = [i for i in variable_names if i]
        indices = [i for i in df.index  if self.findkey(variable_names2, variable_names[i])]

        spans = []
        for inde, i in enumerate(indices):
            start = i
            end = indices[inde+1] if inde < len(indices)-1 else len(df.index)
            # print(start, end)
            span = (start, end)
            spans.append(span)
            # print(span)

        colnames = df.columns
        sel_cols = [i for i in colnames if not self.findkey(['range.+values', 'value.*desc'], i)]
        for start, end in spans:
            if len(range(start, end)) > 2:
                print(f"\n补全 Mortality Codebook {start+1} - {end-2}:", end='\t')
                for col in sel_cols:
                    df[col][start+1:end-1] = df[col][start]
                    print(f"{col}*{end-start-1} OK;", end='\t')

        return df
    def get_nhanes_codebook(self):
        df = self.get_mort_dict_from_pdf()
        df_nhanes = df[df['Notes'].apply(lambda x: self.findkey(['nhanes'], x) or not self.findkey(['nhanes', 'nhis'], x))]
        df_nhanes.index = range(len(df_nhanes.index))
        logstr = (df_nhanes['Variable Name'] == 'UCOD_LEADING') & (df_nhanes['Range of Values'].apply(lambda x: not self.findkey(['001', '002', '010'], x)))
        df_nhanes.loc[logstr, 'drop_cycle'] = '2015-2016-2017-2018'
        df_nhanes['drop_cycle'] = df_nhanes['drop_cycle'].apply(lambda x: self.findkey(['2015-.+-2018'], x, return_value=True))

        return df_nhanes

    def get_file_name_from_update_table(self, path_to):
        if exists(path_to):
            file_ordirs = list(listdir(path_to))
            uptable = [i for i in file_ordirs if self.findkey(['mortality.+[.]update_table'], i)]
            uptable = [i for i in uptable if i]
            uptable = ''.join(uptable)
            if uptable:
                dst = f"{path_to}/{uptable}"
                df = self.read_csv(dst)
                filename = df['File Name'][0].replace('.dat', '')
                return filename
            else:
                return ''

    def cycle_mort_codebook(self, comp, cyl, df_nhanes):
        print(f"\n获取 {cyl} Codebooks：", end='\t')
        path_to = f"{self.dir}/{comp}/{cyl}"
        if exists(path_to):
            df = df_nhanes[df_nhanes['drop_cycle'].apply(lambda x: not self.findkey([cyl], x))]
            df =df[df['Variable Name'].apply(lambda x: True if x else False)]
            df = df.iloc[:, :-1]
            save_path = f"{path_to}/CODEBOOK"
            if not exists(save_path):
                        makedirs(save_path)
            cols = df_nhanes['Variable Name'].unique()
            cols = [i for i in cols if i]
            items = len(cols)
            # file_name = f"{comp} {cyl} items {items}.codebook"
            # save_dst = f"{save_path}/{file_name}"
            file_name = self.get_file_name_from_update_table(path_to)
            save_dst = f"{save_path}/{file_name}.codebook"
            cols2 = ['Sas Label'  if self.findkey(['var.+label'], i) else 'Code or Value' if self.findkey(['range of value'], i) else i  for i in df.columns ]
            df.columns = cols2
            cols3 = [i for i in cols2 if not self.findkey(['var.+type', 'notes'], i)]
            df = df.loc[:, cols3]

            df['Component'] = f'{comp}'
            df['Cycle'] = f'{cyl}'
            df['File Name'] = file_name
            # df['File Name'] = f"{comp}"
            cols4 = ['Component', 'Cycle', 'File Name'] + cols3
            df = df.loc[:, cols4]


            df.to_csv(save_dst, index=False)
            print(f"{file_name.replace('.codebook', '')} OK;", end='\t')

            length_codebooks = len(listdir(save_path))
            try:
                rename(save_path, f"{save_path} items {length_codebooks}")
            except FileExistsError:
                rmtree(f"{save_path} items {length_codebooks}")
                sleep(0.3)
                rename(save_path, f"{save_path} items {length_codebooks}")
    def cycle_other_codebook(self, comp, cyl, max_works=10):
        path_to = f"{self.dir}/{comp}/{cyl}"
        if exists(path_to):

            filenames = listdir(path_to)
            filename = [i for i in filenames if self.findkey(['normal.+.df_normal'], i)]
            if filename:
                filename = ''.join(filename)
                df = self.read_csv(f"{path_to}/{filename}")

                save_path = f"{path_to}/CODEBOOK"
                if not exists(save_path):
                    makedirs(save_path)
                df['doc_name'] = df['Doc File'].apply(lambda x:self.findkey(['^[A-Za-z0-9_]*'], x, return_value=True))
                df['save_dst'] = save_path + '/' + df['doc_name'] + '.codebook'

                if not self.findkey(['demo'], comp):
                    with ThreadPoolExecutor(max_workers=max_works) as poolT:

                        reqs = [poolT.submit(self.get_codebook, comp, cyl, df.iloc[i, :]['doc_name'], df.iloc[i, :]['Doc Web'], df.iloc[i, :]['save_dst']) for i in df.index]
                        for req in reqs:
                            result = req.result()
                            table_list = result[0]
                            save_dst = result[1]

                            file_name = self.findkey(['[A-Za-z0-9_]+.codebook$'], save_dst, return_value=True)
                            try:
                                with open(save_dst, mode='w', newline='', encoding='ANSI') as fp:
                                    csv.writer(fp).writerows(table_list)
                            except UnicodeEncodeError:
                                table_list = self.deal_special_str(table_list, comp)
                                try:
                                    with open(save_dst, mode='w', newline='', encoding='ANSI') as fp:
                                        csv.writer(fp).writerows(table_list)
                                except UnicodeEncodeError:
                                    try:
                                        with open(save_dst, mode='w', newline='', encoding='utf-8') as fp:
                                            csv.writer(fp).writerows(table_list)
                                    except UnicodeEncodeError:
                                        [print(i) for i in table_list]
                            print(f"{file_name.replace('.codebook', '')} OK;", end='\t')
                else:

                    print_list = []
                    for i in df.index:
                        row_series = df.iloc[i, :]
                        file_name = self.findkey(['[A-Za-z0-9_]+'],row_series['Doc File'], return_value=True)
                        web = row_series['Doc Web']
                        save_dst = row_series['save_dst']
                        result = self.get_codebook(comp, cyl, file_name, web, save_dst)
                        table_list = result[0]
                        with open(save_dst, mode='w', encoding='utf-8', newline='') as fp:
                            csv.writer(fp).writerows(table_list)
                        # print(f"{file_name} OK;", end='\t')
                        print_list.append(f"{file_name} OK;")
                    demo_print_srr = '\t'.join(print_list)
                    demo_print_srr = f"获取 {cyl} Codebook:   " + demo_print_srr

                length_codebooks = len(listdir(save_path))
                try:
                    rename(save_path, f"{save_path} items {length_codebooks}")
                except FileExistsError:
                    rmtree(f"{save_path} items {length_codebooks}")
                    sleep(0.3)
                    rename(save_path, f"{save_path} items {length_codebooks}")
                try:
                    return demo_print_srr
                except UnboundLocalError:
                    pass
    def get_codebooks(self, comps=None, cyls=None, max_works=10):
        comps = self.deal_comps(comps)
        cyls = self.deal_cycles(cyls)
        for comp in comps:
            print(f"\n获取 {comp} Codebooks：")
            if self.findkey(['mort'], comp):
                df_nhanes = self.get_nhanes_codebook()

            for cyl in cyls:
                if self.findkey(['mort'], comp):
                    self.cycle_mort_codebook(comp, cyl, df_nhanes)

                else:

                    if self.findkey(['demo'], comp):
                        with ThreadPoolExecutor(max_workers=max_works) as poolT:
                            reqs = [poolT.submit(self.cycle_other_codebook, comp, cyl) for cyl in cyls]
                            for req in reqs:
                                result = req.result()
                                print(result)
                        break
                    else:
                        print(f"\n获取 {cyl} Codebooks：", end='\t')
                        self.cycle_other_codebook(comp, cyl, max_works=max_works)

    def cycle_htm_and_xpt(self, comp, cyl, html=True, xpt=True, max_works=10, start_mtd=5):

        html = 'HTML' if html else ''
        xpt = 'XPT' if xpt else ''
        file_to = [i for i in [html, xpt] if i]
        if file_to:
            for file_type in file_to:
                # print(f"\nStart downloading {file_type}s")
                col = 'Doc Web' if file_type == 'HTML' else 'XPT Web'
                path_to = f"{self.dir}/{comp}/{cyl}"
                if exists(path_to):
                    filenames = listdir(path_to)
                    table_name = [i for i in filenames if self.findkey(['normal.+.df_normal'], i)]

                    if table_name:
                        table_name = ''.join(table_name)
                        df = self.read_csv(f"{path_to}/{table_name}")

                        save_path = f"{path_to}/{file_type}"
                        if not exists(save_path):
                            makedirs(save_path)

                        df['save_dst'] = save_path + '/' + df['Doc File'].apply(lambda x:self.findkey(['[A-Za-z0-9_]*'], x, return_value=True)) + f'.{file_type}'.lower()

                        if not self.findkey(['demo'], comp):
                            with ThreadPoolExecutor(max_workers=max_works) as poolT:
                                if file_type == 'HTML':
                                    reqs = [poolT.submit(self.download_file, df.iloc[i, :][col], df.iloc[i, :]['save_dst'], None, start_mtd) for i in df.index]
                                else:
                                    reqs = [poolT.submit(self.download_file, df.iloc[i, :][col], df.iloc[i, :]['save_dst'], df.iloc[i, :]['Data File'], start_mtd) for i in df.index]
                                for req in reqs:
                                    result = req.result()
                                    print(f"{result} OK;", end='\t')
                                print('')
                        else:
                            print_list = []
                            for i in df.index:
                                row_series = df.iloc[i, :]
                                file_name = self.findkey(['[A-Za-z0-9_]+'],row_series['Doc File'], return_value=True)
                                web = row_series['Doc Web'] if file_type == 'HTML' else row_series['XPT Web']
                                save_dst = row_series['save_dst']

                                data_file = None if file_type == 'HTML' else row_series['Data File']
                                self.download_file(web, save_dst, data_file, start_mtd)

                                print_list.append(f"{file_name} OK;")
                            demo_print_srr = '\t'.join(print_list)
                            demo_print_srr = f"获取 {cyl} {file_type}:   " + demo_print_srr
                        length_codebooks = len(listdir(save_path))

                        try:
                            rename(save_path, f"{save_path} items {length_codebooks}")
                        except FileExistsError:
                            rmtree(f"{save_path} items {length_codebooks}")
                            sleep(0.3)
                            rename(save_path, f"{save_path} items {length_codebooks}")

                        try:
                            return demo_print_srr
                        except UnboundLocalError:
                            pass
    def cycle_mort_dat(self, comp, cyl):
        file_type = 'DAT'
        path_to = f"{self.dir}/{comp}/{cyl}"
        if exists(path_to):
            filenames = listdir(path_to)
            table_name = [i for i in filenames if self.findkey(['normal.+.df_normal'], i)]
            if table_name:
                table_name = ''.join(table_name)
                df = self.read_csv(f"{path_to}/{table_name}")

                save_path = f"{path_to}/{file_type}"
                if not exists(save_path):
                    makedirs(save_path)

                df['save_dst'] = save_path + '/' + df['File Name']

                print_list = []
                for i in df.index:
                    row_series = df.iloc[i, :]
                    file_name = row_series['File Name']
                    web = row_series['File Web']
                    save_dst = row_series['save_dst']
                    self.download_file(web, save_dst)

                    print_list.append(f"{file_name} OK;")
                demo_print_srr = '\t'.join(print_list)
                demo_print_srr = f"获取 {cyl} {file_type}:   " + demo_print_srr

                length_codebooks = len(listdir(save_path))
                rename(save_path, f"{save_path} items {length_codebooks}")
        return demo_print_srr
    def download_files(self, comps=None, cyls=None, html=True, xpt=True, max_works=10,  start_mtd=5):
        comps = self.deal_comps(comps)
        cyls = self.deal_cycles(cyls)
        html = 'HTML' if html else ''
        xpt = 'XPT' if xpt else ''
        file_to = [i for i in [html, xpt] if i]
        file_types = ' and '.join(file_to) if len(file_to)>1 else ''.join(file_to)
        if file_to:
            for comp in comps:
                print(f"\n获取 {comp} {file_types} files：")

                for cyl in cyls:
                    if self.findkey(['mort'], comp):
                        file_type = 'DAT'
                        print(f"\nStart downloading {file_type} ==>")
                        with ThreadPoolExecutor(max_workers=max_works) as poolT:
                            reqs = [poolT.submit(self.cycle_mort_dat, comp, cyl) for cyl in cyls]
                            for req in reqs:
                                result = req.result()
                                print(result)
                        break
                    else:
                        for file_type in file_to:
                            html = True if file_type == 'HTML' else False
                            xpt = True if file_type == 'XPT' else False

                            if self.findkey(['demo'], comp):
                                print(f"\nStart downloading {file_type} ==>")
                                with ThreadPoolExecutor(max_workers=max_works) as poolT:
                                    reqs = [poolT.submit(self.cycle_htm_and_xpt, comp, cyl, html, xpt, max_works, start_mtd) for cyl in cyls]
                                    for req in reqs:
                                        result = req.result()
                                        print(result)

                            else:

                                print(f"\nStart downloading {file_type} ==>")
                                print(f"获取 {cyl} {file_type}：", end='\t')
                                self.cycle_htm_and_xpt(comp, cyl, html, xpt, max_works)

                        if self.findkey(['demo'], comp):
                            break

    def download_thread(self, web, dst, result=None, thread='many'):
        if thread == 'many':
            MTD().run(web, dst)
        elif thread == 'single':
            self.single_thread_download(web, dst)
        if result is not None:
            return result
    def cycle_dxx_vid(self, comp, cyl, file, thread):
        path_to = f"{self.dir}/{comp}/{cyl}"
        error_to = listdir(path_to)
        error_to = [i for i in error_to if self.findkey(['error[^.]+$'], i)]
        error_to = [i for i in error_to if i]
        if error_to:
            error_to = ''.join(error_to)

            path_to = f"{path_to}/{error_to}"
            dxx_to = listdir(path_to)
            dxx_to = [i for i in dxx_to if self.findkey([f'{file}.*'], i)]
            dxx_to = [i for i in dxx_to if i]
            if dxx_to:
                dxx_to = ''.join(dxx_to)

                path_to = f"{path_to}/{dxx_to}"
                table_to = listdir(path_to)
                table_to = [i for i in table_to if self.findkey([f'{file}.+.update_table'], i)]
                table_to = [i for i in table_to if i]
                if table_to:
                    print(f"\n获取 {cyl} {file.upper()} files：")
                    for i in table_to:
                        dxx_table_dst = f"{path_to}/{i}"
                        df = self.read_csv(dxx_table_dst)
                        cycle_list = []
                        for i in df.index:

                            doc_file = df.iloc[i, :]['Doc File']
                            doc_name = self.findkey([f'{file}[_A-Za-z0-9]*'], doc_file, return_value=True)
                            doc_web = df.iloc[i, :]['Doc Web']
                            suffix = self.findkey(['[pdfhtml]+$'], doc_web, return_value=True)
                            suffix = 'html' if self.findkey(['htm'], suffix) else suffix.lower()
                            save_to = f"{path_to}/{suffix.upper()}"
                            if not exists(save_to):
                                makedirs(save_to)
                            doc_save_dst = f"{save_to}/{doc_name}.{suffix}"
                            cycle_list.append([doc_web, doc_save_dst, f"{doc_name}.{suffix}", save_to])

                            file_web = df.iloc[i, :]['XPT Web']
                            suffix2 = self.findkey(['[xptzip]+$'], file_web, return_value=True)
                            suffix2 = suffix2.lower()
                            save_to2 = f"{path_to}/{suffix2.upper()}"
                            if not exists(save_to2):
                                makedirs(save_to2)
                            file_save_dst = f"{save_to2}/{doc_name}.{suffix2}"
                            cycle_list.append([file_web, file_save_dst, f"{doc_name}.{suffix2}", save_to2])
                        with ThreadPoolExecutor(max_workers=2) as poolT:
                            reqs = [poolT.submit(self.download_thread, web, dst, doc_name, thread) for web, dst, doc_name, save_to in cycle_list]
                            for req in reqs:
                                result = req.result()
                                print(f"{result} OK;", end='\t')
                            print('')
                        for web, dst, doc_name, save_to in cycle_list:
                            items = len(listdir(save_to))
                            try:
                                rename(save_to, f"{save_to} items {items}")
                            except FileExistsError:
                                rmtree(f"{save_to} items {items}")
                                sleep(0.3)
                                rename(save_to, f"{save_to} items {items}")

    def download_error_dxx(self, cyls=None, file='dxx'):
        cyls = self.deal_cycles(cyls)

        comps = listdir(self.dir)
        comp = [i for i in comps if self.findkey(['exam'], i)]
        comp = [i for i in comp if i]
        comp = ''.join(comp) if comp else 'Examination'
        for cyl in cyls:
            self.cycle_dxx_vid(comp, cyl, file, thread='many')
    def download_error_vid(self, cyls=None, file='vid'):
        cyls = self.deal_cycles(cyls)

        comps = listdir(self.dir)
        comp = [i for i in comps if self.findkey(['lab'], i)]
        comp = [i for i in comp if i]
        comp = ''.join(comp) if comp else 'Laboratory'
        for cyl in cyls:
            self.cycle_dxx_vid(comp, cyl, file, thread='single')



    def read_sas(self, open_dst):
        data_open_name = self.findkey(['[^/]+$'], open_dst, return_value=True)
        # print(open_dst)
        encoding_list = ['utf-8', 'ANSI', None, 'GBK', 'GB18030']
        for inde, encode in enumerate(encoding_list):
            try:
                df = read_sas(open_dst, encoding=encode)
                # print(f'{names_xpt} decoding from {encode}')
                break
            except UnicodeError:
                if inde == len(encoding_list)-1:
                    print(f'{data_open_name} decoding failed!')
                continue
        return df
    def read_csv(self, open_dst):
        data_open_name = self.findkey(['[^/]+$'], open_dst, return_value=True)
        # print(open_dst)
        encoding_list = ['utf-8', 'ANSI', None, 'GBK', 'GB18030']
        for inde, encode in enumerate(encoding_list):
            try:
                df = read_csv(open_dst, encoding=encode)
                # print(f'{names_xpt} decoding from {encode}')
                break
            except UnicodeError:
                if inde == len(encoding_list)-1:
                    print(f'{data_open_name} decoding failed!')
                continue
        return df
    def trans_xpt(self, open_dst, save_dst):
        data_save_name = self.findkey(['[^/]+$'], save_dst, return_value=True)
        df = self.read_sas(open_dst)
        df = df.replace(5.397605346934028e-79, 0)
        df.to_csv(save_dst, index=False)
        return f"{data_save_name}"
    def read_dat(self, open_dst):
        data_open_name = self.findkey(['[^/]+$'], open_dst, return_value=True)
        if self.findkey(['nhanes'], data_open_name):
            colnames = ['seqn', 'space1', 'eligstat', 'mortstat', 'ucod_leading', 'diabetes', 'hyperten', 'space2', 'permth_int', 'permth_exm']
            colnames = [ i.upper() for i in colnames]
            widths = [ 6,         8,      1,1,3,1,1 ,     21,      3,3]
        elif self.findkey(['nhis'], data_open_name):
            colnames = ['publicid', 'eligstat', 'mortstat', 'ucod_leading', 'diabetes', 'hyperten', 'dodqtr',
                        'dodyear', 'wgt_new', 'sa_wgt_new']
            colnames = [i.upper() for i in colnames]
            widths = [14, 1, 1, 3, 1, 1, 1, 4, 8, 8]

        df = read_fwf(open_dst, header=None, names=colnames, widths= widths)
        df = df.loc[:, [i for i in df.columns if not self.findkey(['space'], i)]]
        df = df.replace(".", nan)
        return df
    def trans_dat(self, open_dst, save_dst):
        data_save_name = self.findkey(['[^/]+$'], save_dst, return_value=True)
        df = self.read_dat(open_dst)
        df.to_csv(save_dst, index=False)
        return f"{data_save_name}"
    def cycle_data_to_csv(self, comp, cyl, max_works=10):
        file_type = 'XPT' if not self.findkey(['mort'], comp) else 'DAT'
        path_to = f"{self.dir}/{comp}/{cyl}"

        if exists(path_to):
            dirs = listdir(path_to)
            data_file_read = [i for i in dirs if self.findkey([f'{file_type} *items *[0-9]+'], i)]
            if data_file_read:
                data_file_read = ''.join(data_file_read) if len(data_file_read) <2 else ''.join(data_file_read[0])
                data_file_read = f"{path_to}/{data_file_read}"
                data_files = listdir(data_file_read)
                data_file_names = [i for i in data_files if self.findkey([f'.+.{file_type}$'], i)]
                if data_file_names:
                    save_path = f"{path_to}/CSV"
                    if not exists(save_path):
                        makedirs(save_path)

                    open_dsts = [f"{data_file_read}/{i}" for i in data_file_names]
                    data_file_names = [self.findkey(['^[^.]+'], i, return_value=True) for i in data_file_names]
                    save_dsts = [f"{save_path}/{i}.csv" for i in data_file_names]

                    if self.findkey(['mort'], comp):
                        print_list = []
                        for open_dst, save_dst in zip(open_dsts, save_dsts):

                            file_name = self.findkey(['[^/]+$'], save_dst, return_value=True)
                            self.trans_dat(open_dst, save_dst)

                            print_list.append(f"{file_name} OK;")
                        demo_print_srr = '\t'.join(print_list)
                        demo_print_srr = f"转换 {cyl} {file_type}:   " + demo_print_srr
                    else:
                        if not self.findkey(['demo'], comp):

                            with ThreadPoolExecutor(max_workers=max_works) as poolT:
                                reqs = [poolT.submit(self.trans_xpt, open_dst, save_dst) for open_dst, save_dst in zip(open_dsts, save_dsts)]
                                for req in reqs:
                                    result = req.result()
                                    print(f"{result} OK;", end='\t')
                                print('')
                        else:
                            print_list = []
                            for open_dst, save_dst in zip(open_dsts, save_dsts):

                                file_name = self.findkey(['[^/]+$'], save_dst, return_value=True)
                                self.trans_xpt(open_dst, save_dst)

                                print_list.append(f"{file_name} OK;")
                            demo_print_srr = '\t'.join(print_list)
                            demo_print_srr = f"转换 {cyl} {file_type}:   " + demo_print_srr

                    length_csv = len(listdir(save_path))
                    try:
                        rename(save_path, f"{save_path} items {length_csv}")
                    except FileExistsError:
                        rmtree(f"{save_path} items {length_csv}")
                        sleep(0.3)
                        rename(save_path, f"{save_path} items {length_csv}")

                    try:
                        return demo_print_srr
                    except UnboundLocalError:
                        pass

    def trans_XPTs(self, comps=None, cyls=None, max_works=10):
        comps = self.deal_comps(comps)
        cyls = self.deal_cycles(cyls)

        for comp in comps:
            file_type = 'XPT' if not self.findkey(['mort'], comp) else 'DAT'
            print(f"\n转换 {comp} {file_type} files：")
            # if self.findkey(['mort'], comp):
            #     df_nhanes = self.get_nhanes_codebook()

            for cyl in cyls:
                if self.findkey(['mort'], comp):
                    with ThreadPoolExecutor(max_workers=max_works) as poolT:
                            reqs = [poolT.submit(self.cycle_data_to_csv, comp, cyl, max_works) for cyl in cyls]
                            for req in reqs:
                                result = req.result()
                                print(result)
                    break

                else:

                    if self.findkey(['demo'], comp):
                        with ThreadPoolExecutor(max_workers=max_works) as poolT:
                            reqs = [poolT.submit(self.cycle_data_to_csv, comp, cyl, max_works) for cyl in cyls]
                            for req in reqs:
                                result = req.result()
                                print(result)
                        break
                    else:
                        print(f"\n转换 {cyl} {file_type}：", end='\t')
                        self.cycle_data_to_csv(comp, cyl, max_works=max_works)

    def factor_level(self, x, level, label):
        if x.isdigit() and  float():
            return label
        else:
            return x

    def factor_csv(self, csv_file_dst, codebook_file_dst):
        csv_name = self.findkey(['[^\/]+$'], csv_file_dst, return_value=True)
        print(f"处理 {csv_name}:")
        df_csv = self.read_csv(csv_file_dst)
        df_cob = self.read_csv(codebook_file_dst)

        file_from = self.findkey(['.+mort.+public'], csv_file_dst)
        factor_num = 0
        for col in df_csv.columns:
            df_col_cob= df_cob[df_cob['Variable Name'] == col]
            df_col_cob = df_col_cob[df_col_cob['Variable Name'] != 'SEQN']
            df_col_cob = df_col_cob[df_col_cob['Value Description'].apply(lambda x: not self.findkey(['missing', 'range of value', 'fill value', 'value was recorded'], x))]



            if not df_col_cob.empty:
                if file_from:
                    df_col_cob = df_col_cob[df_col_cob['Code or Value'].apply(lambda x: not self.findkey(['nan', '\.', '[0-9]+-[0-9]+'], x))]

                if not df_col_cob.empty:
                    factor_num += 1
                    levels = df_col_cob['Code or Value'].values #if not file_from else df_col_cob['Range of Values'].values
                    labels = df_col_cob['Value Description'].values
                    print(f"factor {col}:", end='\t')
                    for level, label in zip(levels, labels):
                        try:
                            level = float(level)
                            df_csv.loc[df_csv[col] == level , col] = label
                        except ValueError:
                            df_csv.loc[df_csv[col] == level , col] = label
                        print(f"{level} ==> {label};", end='\t')
                    print('')
        if factor_num == 0:
            print(f" no need to factor as a numeric vector")
        return [df_csv, factor_num]
    def cycle_factor_csv(self, file, path_to, path_csv, path_codebook):
        # csv_files = listdir(path_csv)
        codebook_files = listdir(path_codebook)
        doc_name = self.findkey(['^[^.]+'], file, return_value=True)
        cob_name = f"{doc_name}.codebook"

        file_from = self.findkey(['.+mort.+codebook.+'], path_codebook)
        if file_from:
            cob_name = [i for i in codebook_files if self.findkey(['mort.+.codebook'], i)]
            cob_name = [i for i in cob_name]
            if cob_name:
                cob_name = ''.join(cob_name)


        save_path = f"{path_to}/FACTOR CSV"

        if cob_name not in codebook_files:
            print(f"{doc_name} 没有 codebook 解码文件！")
            csv_file_dst = f"{path_csv}/{file}"
            df_cob = self.read_csv(csv_file_dst)
            save_dst = f"{save_path}/{doc_name}_factor{0}.csv"
        else:
            csv_file_dst = f"{path_csv}/{file}"
            codebook_file_dst = f"{path_codebook}/{cob_name}"
            result = self.factor_csv(csv_file_dst, codebook_file_dst)
            df_cob = result[0]
            factor_num = result[1]
            save_dst = f"{save_path}/{doc_name}_{factor_num}.csv"

        df_cob.to_csv(save_dst)
        return f"{doc_name} OK;"

    def code_CSVs(self, comps=None, cyls=None, max_works=10):
        comps = self.deal_comps(comps)
        cyls = self.deal_cycles(cyls)

        for comp in comps:

            for cyl in cyls:
                path_to = f"{self.dir}/{comp}/{cyl}"
                if exists(path_to):
                    dirs = listdir(path_to)
                    dir_csv = [i for i in dirs if self.findkey(['^csv *items *[0-9]+'], i)]
                    dir_csv = [i for i in dir_csv if i]

                    dir_codebook = [i for i in dirs if self.findkey(['^codebook *items *[0-9]+'], i)]
                    dir_codebook = [i for i in dir_codebook if i]
                    print(dir_codebook)
                    if not dir_csv:
                        print(f"{comp} {cyl} 缺少 CSV文件夹！")
                    elif not dir_codebook:
                        print(f"{comp} {cyl} 缺少 CODEBOOK文件夹！")
                    elif not dir_codebook and not dir_csv:
                        print(f"{comp} {cyl} 缺少 CSV and CODEBOOK文件夹！")
                    elif dir_csv and dir_codebook:
                        dir_csv = ''.join(dir_csv)
                        dir_codebook = ''.join(dir_codebook)

                        path_csv = f"{path_to}/{dir_csv}"
                        path_codebook = f"{path_to}/{dir_codebook}"
                        csv_files = listdir(path_csv)
                        codebook_files = listdir(path_codebook)

                        save_path = f"{path_to}/FACTOR CSV"
                        if not exists(save_path):
                            makedirs(save_path)
                        print(f"\n----------{comp} {cyl} CSVs = {len(csv_files)}; CODEBOOKS = {len(codebook_files)};----------")

                        with ThreadPoolExecutor(max_workers=max_works) as poolT:
                            reqs = [poolT.submit(self.cycle_factor_csv, file, path_to, path_csv, path_codebook) for file in csv_files]
                            for req in reqs:
                                result = req.result()
                                print(f"{result}")

                        length_csv = len(listdir(save_path))
                        try:
                            rename(save_path, f"{save_path} items {length_csv}")
                        except FileExistsError:
                            rmtree(f"{save_path} items {length_csv}")
                            sleep(0.3)
                            rename(save_path, f"{save_path} items {length_csv}")


class MTD:
    def __init__(self, num=10):
        self.num = num              # 线程数,默认10
        self.url = ''               # url
        self.name = ''              # 目标地址
        self.total = 0              # 文件大小
    # 获取每个线程下载的区间
    def get_range(self):
        ranges = []
        # print(self.num)
        # print(self.total)
        offset = int(self.total/self.num)
        for i in range(self.num):
            if i == self.num-1:
                ranges.append((i*offset, ''))
            else:
                ranges.append(((i * offset), (i + 1) * offset - 1))

        # [print(i)  for i in ranges]
        return ranges               # [(0,99),(100,199),(200,"")]
    # 通过传入开始和结束位置来下载文件
    def download(self, ts_queue):
        while not ts_queue.empty():
            start_, end_ = ts_queue.get()
            headers = {
                'Range': 'Bytes=%s-%s' % (start_, end_),
                'Accept-Encoding': '*',
                "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:101.0) Gecko/20100101 Firefox/101.0"
                }
            flag = False
            while not flag:
                try:
                    # 设置重连次数
                    adapters.DEFAULT_RETRIES = 10
                    # s = requests.session()            # 每次都会发起一次TCP握手,性能降低，还可能因发起多个连接而被拒绝
                    # # 设置连接活跃状态为False
                    # s.keep_alive = False
                    # 默认stream=false,立即下载放到内存,文件过大会内存不足,大文件时用True需改一下码子
                    res = get(self.url, headers=headers)
                    res.close()                         # 关闭请求  释放内存
                except Exception as e:
                    print((start_, end_, "出错了,连接重试:%s", e, ))
                    time.sleep(1)
                    continue
                flag = True

            # print("\r", ("%s-%s download success" % (start_, end_)),  flush=True)
            # with lock:
            with open(self.name, "rb+") as fd:
                fd.seek(start_)
                fd.write(res.content)

            # self.fd.seek(start_)                                        # 指定写文件的位置,下载的内容放到正确的位置处
            # self.fd.write(res.content)                                  # 将下载文件保存到 fd所打开的文件里
    def set_num(self,total):
        num= 4 if total<512*1024\
            else 8 if total<1024*1024\
            else 16 if total <3*1024*1024\
            else 32
        # print(num)
        self.num=num

        return total
    def set_total(self, url):
        total = int(head(url).headers['Content-Length'])
        self.total = total
        return total
    def run(self, url, file_path):
        self.url = url
        self.name = file_path
        total = self.set_total(url)
        self.set_num(self.total)
        # file_size = int(urlopen(self.url).info().get('Content-Length', -1))
        file_size = self.total
        if exists(file_path):
            first_byte = getsize(file_path)
        else:
            first_byte = 0
        if first_byte >= file_size:
            return file_size

        self.fd = open(file_path, "wb")                   # 续传时直接rb+ 文件不存在时会报错,先wb再rb+
        self.fd.truncate(self.total)                 # 建一个和下载文件一样大的文件,不是必须的,stream=True时会用到
        self.fd.close()
        # self.fd = open(self.name, "rb+")           # 续传时ab方式打开时会强制指针指向文件末尾,seek并不管用,应用rb+模式
        thread_list = []
        ts_queue = Queue()                           # 用队列的线程安全特性，以列表的形式把开始和结束加到队列
        for ran in self.get_range():
            start_, end_ = ran
            ts_queue.put((start_, end_))
        for i in range(self.num):
            t = Thread(target=self.download, name='th-' + str(i), kwargs={'ts_queue': ts_queue})
            t.setDaemon(True)
            thread_list.append(t)
        for t in thread_list:
            t.start()
        name_show = file_path.split('/')[-1]
        for t in thread_list:
            t.join()
            # 设置等待，全部线程完事后再继续

        self.fd.close()


if __name__ == '__main__':
    nhanes_D = nhanes_d()
