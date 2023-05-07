from nhanes_d import nhanes_d
from pandas import read_csv, concat, merge, DataFrame
from numpy import nan, isnan, percentile
from os import makedirs, listdir
from os.path import exists
from re import findall
from pyperclip import copy


class nhanes_v(nhanes_d):

    def __init__(self):
        super().__init__()

    def combind_codebooks(self, comps=None, cyls=None, save_df=False):
        comps = self.deal_comps(comps)
        cyls = self.deal_cycles(cyls)
        for comp in comps:
            print(f"\n----------合并 {comp} codebooks----------")
            for cyl in cyls:
                path_to = f"{self.dir}/{comp}/{cyl}"
                print(f"合并 {cyl} codebooks:", end='\t')
                # print(path_to)
                if exists(path_to):
                    dirs = listdir(path_to)
                    dir_codebook = [i for i in dirs if self.findkey(['^codebook *items *[0-9]+'], i)]
                    dir_codebook = [i for i in dir_codebook if i]

                    if dir_codebook:
                        dir_codebook = ''.join(dir_codebook)
                        path_codebook = f"{path_to}/{dir_codebook}"
                        codebook_files = listdir(path_codebook)
                        codebook_files = [i for i in codebook_files if self.findkey(['.+.codebook'], i)]
                        codebook_files = [i for i in codebook_files if i]
                        if codebook_files:
                            cob_dsts = [f"{path_codebook}/{cob}" for cob in codebook_files]

                            df_cob_list = []
                            for cob_dst in cob_dsts:
                                doc_name = self.findkey(['[^/]+$'], cob_dst, return_value=True)
                                df_cob = self.read_csv(cob_dst)
                                df_cob_list.append(df_cob)
                                print(f"{doc_name} OK;", end='\t')
                            df_all = concat(df_cob_list, join='outer',axis=0)
                            if save_df:
                                length_files = df_all['File Name'].unique() #if not self.findkey(['mort'], comp) else [1]
                                length_files = [i for i in length_files if i and not self.findkey(['nan'], i)]
                                length_files = len(length_files)

                                length_vars = df_all['Variable Name'].unique()
                                length_vars = [i for i in length_vars if i and not self.findkey(['nan'], i)]
                                length_vars = len(length_vars)

                                save_name = f"{comp} {cyl} files {length_files}; vars {length_vars}.codebook_all"
                                save_dst = f"{path_to}/{save_name}"
                                df_all.to_csv(save_dst, index=False)
                            print('')

    def find_combined_codebooks(self, comps=None, cyls=None):
        comps = self.deal_comps(comps)
        cyls = self.deal_cycles(cyls)
        path2 = []
        for comp in comps:

            for cyl in cyls:
                path_to = f"{self.dir}/{comp}/{cyl}"
                if exists(path_to):
                    dirs = listdir(path_to)
                    dir_codebook_all = [i for i in dirs if self.findkey(['.+.codebook_all'], i)]
                    dir_codebook_all = [i for i in dir_codebook_all if i]
                    if dir_codebook_all:
                        dir_codebook_all = ''.join(dir_codebook_all)
                        dst = f"{path_to}/{dir_codebook_all}"

                        path2.append([comp, cyl, dst])
        return path2

    def bind_codebooks_founded(self, comps=None, cyls=None):
        paths = self.find_combined_codebooks(comps=comps, cyls=cyls)
        table_list = []
        for i in paths:
            comp = i[0]
            # cyl = i[1]
            dst = i[-1]
            table = self.read_csv(dst)
            table_list.append(table)
        df_list = concat(table_list, join='outer', axis=0)
        df_list.index = range(len(df_list.index))
        df_list['Variable Name'] = df_list['Variable Name'].str.replace(' ', '')
        df_list['Variable Name'] = df_list['Variable Name'].str.upper()
        return df_list

    def deal_title_names(self, title_names=None):
        title_names = [title_names] if type(title_names)==str or type(title_names)==int or type(title_names)==float else title_names
        return title_names


    def find_keys(self, keys_or=None, keys_not=None, comps=None, cyls=None, sas_labels=None, file_names=None, variable_names=None, code_or_value=None, value_description=None, data_df=None, lower=True):

        df = self.bind_codebooks_founded( comps=comps, cyls=cyls) if data_df is None else data_df


        keys_all = [sas_labels, file_names, variable_names, code_or_value, value_description]
        colnames = ['Sas Label', 'File Name', 'Variable Name', 'Code or Value', 'Value Description']
        for keys, variable_name in list(zip(keys_all, colnames)):
            if keys is not None:
                keys = self.deal_title_names(keys)
                df = df[df[variable_name].apply(lambda x: self.findkey(keys, x, lower=lower))]


        for keys, assoc in list(zip([keys_or, keys_not], ['or', 'not'])):
            if keys is not None:
                keys = self.deal_title_names(keys)
                ifs =   (df['Component'].apply(lambda x: self.findkey(keys, x, lower=lower)) |
                        df['Cycle'].apply(lambda x: self.findkey(keys, x, lower=lower)) |
                        df['File Name'].apply(lambda x: self.findkey(keys, x, lower=lower)) |
                        df['Variable Name'].apply(lambda x: self.findkey(keys, x, lower=lower)) |
                        df['Sas Label'].apply(lambda x: self.findkey(keys, x, lower=lower)) |
                        df['Code or Value'].apply(lambda x: self.findkey(keys, x, lower=lower))|
                        df['Value Description'].apply(lambda x: self.findkey(keys, x, lower=lower)) )
                if assoc == 'not':
                    ifs = ~ifs
                df = df[ifs]
        df.index = range(len(df.index))
        return  df

    def match_key(self, variable_name, renames):
        find_var = [(a, b) for a, b in renames if self.findkey([a], variable_name)]
        if len(find_var) > 1:
            diff = min([abs(len(a) - len(variable_name)) for a, b in find_var])
            find_var = [(a, b) for a, b in find_var if abs(len(a) - len(variable_name)) == diff]
        else:
            find_var = [(a, b) for a, b in find_var] if find_var else find_var

        return find_var
    def reanmes(self, renames, x, upper=True):
        if upper:
            renames = [(f'{value1}'.upper(), value2) for value1, value2 in renames]

        find_var = self.match_key(variable_name=x, renames=renames)
        if find_var:
            for value1, value2 in find_var:
                value = f"{x} ==> {value2}"
        else:
            value = x
        return value


    def deal_results(self, df, renames=None, upper=True):
        df = df.drop_duplicates(subset=['Component', 'Cycle', 'File Name', 'Variable Name'])
        df = df.loc[:, ['Component', 'Cycle', 'File Name', 'Variable Name', 'Sas Label']]
        df['Variable Name'] = df['Variable Name'].str.replace(' ', '')
        df = df.loc[df['Variable Name'] != 'SEQN' , :]
        df.index = range(len(df))
        variables = df['Variable Name'].unique()
        variables = list(zip(variables, ['']*len(variables)))
        print(f"renames={variables}")
        copy(f", renames={variables}")
        if renames is not None:
            df['Variable Name'] = df['Variable Name'].apply(lambda x: self.reanmes(renames, x, upper=upper))
        return df

    def read_csv(self, dst):
        encoding_list = [None, 'ANSI', 'utf-8',  'GBK', 'GB18030']
        df = None
        for inde, encoding in enumerate(encoding_list):
            try:
                df = read_csv(dst, encoding=encoding)
                break
            except:
                if inde != len(encoding_list) - 1:
                    continue
                else:
                    print(f'{dst} reading failed!')
        return df

    def get_codebook_name(self, x):
        info = x.split(' ==> ')
        path = info[0]
        name = info[1].replace('.csv', '').replace('.CSV', '')
        file_name = [i for i in listdir(path) if findall(f'{name}.+', i)]
        file_name = path + ''.join(file_name)
        return file_name

    def find_path_csv(self, path_to):
        # key = ['^csv *items *[0-9]+'] if not self.findkey(['Mortality'], path_to) else ['']
        if exists(path_to):
            dirs = listdir(path_to)
            dir_csv = [i for i in dirs if self.findkey(['^csv *items *[0-9]+'], i)]
            dir_csv = [i for i in dir_csv if i]
            dir_csv = ''.join(dir_csv)
        return dir_csv
    def find_path_codebook(self, path_to):
        if exists(path_to):
            dirs = listdir(path_to)
            dir_codebook = [i for i in dirs if self.findkey(['^codebook *items *[0-9]+'], i)]
            dir_codebook = [i for i in dir_codebook if i]
            dir_codebook = ''.join(dir_codebook)
        return dir_codebook
    def find_path_factor_csv(self, path_to):
        if exists(path_to):
            dirs = listdir(path_to)
            dir_codebook = [i for i in dirs if self.findkey(['^factor *CSV *items *[0-9]+'], i)]
            dir_codebook = [i for i in dir_codebook if i]
            dir_codebook = ''.join(dir_codebook)
        return dir_codebook
    def find_csv_file(self, path_csv, filename):
        files = listdir(path_csv)
        file_name = [i for i in files if self.findkey([f"^{filename}.csv"], i)]
        file_name = [i for i in file_name if i]
        file_name = ''.join(file_name)
        return file_name
    def find_codebook_file(self, path_codebook, filename):
        files = listdir(path_codebook)
        file_name = [i for i in files if self.findkey([f"^{filename}.codebook"], i)]
        file_name = [i for i in file_name if i]
        file_name = ''.join(file_name)
        return file_name
    def find_factor_csv_file(self, path_factor_csv, filename):
        files = listdir(path_factor_csv)
        file_name = [i for i in files if self.findkey([f"^{filename}_[\d]+.csv"], i)]
        file_name = [i for i in file_name if i]
        file_name = ''.join(file_name)
        return file_name
    def factor_data(self, data, codebook_file_dst):
        csv_name = self.findkey(['[^\/]+$'], codebook_file_dst, return_value=True).replace('.codebook', '')
        print(f"处理 {csv_name}:")
        df_cob = read_csv(codebook_file_dst)
        df_cob['Variable Name'] = df_cob['Variable Name'].str.replace(' ', '')
        df_cob['Variable Name'] = df_cob['Variable Name'].str.upper()

        file_from = self.findkey(['^mortality.+'], codebook_file_dst)
        factor_num = 0
        for col in data.columns:
            df_col_cob= df_cob[df_cob['Variable Name'] == col]
            df_col_cob = df_col_cob[df_col_cob['Variable Name'] != 'SEQN']
            df_col_cob = df_col_cob[df_col_cob['Value Description'].apply(lambda x: not self.findkey(['missing', 'range of value', 'fill value', 'value was recorded'], x))]

            if not df_col_cob.empty:
                if file_from:
                    df_col_cob = df_col_cob[df_col_cob['Code or Value'].apply(lambda x: not self.findkey(['nan', '\.', '[0-9]+-[0-9]+'], x))]

                if not df_col_cob.empty:
                    factor_num += 1
                    levels = df_col_cob['Code or Value'].values
                    labels = df_col_cob['Value Description'].values
                    print(f"factor {col}:", end='\t')
                    for level, label in zip(levels, labels):
                        try:
                            level = float(level)
                            data.loc[data[col] == level , col] = label
                        except ValueError:
                            data.loc[data[col] == level , col] = label
                        print(f"{level} ==> {label};", end='\t')
                    print('')
        if factor_num == 0:
            print(f" no need to factor as a numeric vector")
        return data
    def combine_files2(self, df, codebook=False):
        df['path_to'] = self.dir + '/' + df['Component'] + '/' + df['Cycle']
        df['path_csv'] = df['path_to'].apply(lambda x: self.find_path_csv(x))
        df['path_codebook'] = df['path_to'].apply(lambda x: self.find_path_codebook(x))
        df['path_factor_csv'] = df['path_to'].apply(lambda x: self.find_path_factor_csv(x))

        path_tos = df['path_to'].values
        path_csvs = df['path_csv'].values
        path_codebooks = df['path_codebook'].values
        path_factor_csvs = df['path_factor_csv'].values

        path_csvs = [f"{a}/{b}" if a and b else '' for a, b in zip(path_tos, path_csvs) ]
        path_codebooks = [f"{a}/{b}" if a and b else '' for a, b in zip(path_tos, path_codebooks) ]
        path_factor_csvs = [f"{a}/{b}" if a and b else '' for a, b in zip(path_tos, path_factor_csvs) ]
        file_names = df['File Name'].values

        csv_names = [self.find_csv_file(a, b) for a, b in zip(path_csvs, file_names)]
        codebook_names = [self.find_codebook_file(a, b) for a, b in zip(path_codebooks, file_names)]
        factor_csv_names = [self.find_factor_csv_file(a, b) for a, b in zip(path_factor_csvs, file_names)]

        df['csv_name'] = csv_names
        df['codebook_name'] = codebook_names
        df['factor_csv_name'] = factor_csv_names
        if not codebook:

            df['dst'] = df['path_to'] + '/' + df['path_csv'] + '/' + df['csv_name']
        else:
            df['dst'] = df['path_to'] + '/' + df['path_factor_csv'] + '/' + df['factor_csv_name']


        df['sas_name'] = df['Variable Name'].apply(lambda x: x.split(' ==> ')[0].upper())
        df['rename'] = df['Variable Name'].apply(lambda x: x.split(' ==> ')[1] if len(x.split(' ==> ')) > 1 else x.split(' ==> ')[0].upper())
        df_concat = []
        df_merge = None
        # return df
        concat_list = []
        for cyl in df['Cycle'].unique():
            df_cyl = df[df['Cycle'] == cyl]
            df_cyl.index = range(len(df_cyl))

            merge_list = []
            for inde, file in enumerate(df_cyl['File Name'].unique()):
                df_file = df_cyl[df_cyl['File Name'] == file]
                df_file.index = range(len(df_file))

                dst = df_file['dst'].values[0]
                # print(dst, inde)
                data = self.read_csv(dst)
                sel_cols = ['SEQN'] + list(df_file['sas_name'])
                renames = ['SEQN'] + list(df_file['rename'])
                try:
                    data2 = data.loc[:, sel_cols]
                except KeyError:
                    sel_cols = [sasname.replace('OSQ', 'OSD') if sasname.find('OSQ030') !=-1 else sasname for sasname in sel_cols]
                    data2 = data.loc[:, sel_cols]
                    print(f'{cyl}  OSQ030 replaced to OSD030 !')
                data2.columns = renames
                if inde==0:
                    df_merge = data2
                else:
                    df_merge = merge(df_merge, data2, how='outer', on=['SEQN'])
                merge_list.append(f'{file}')
                variable_name_list = [sasname  if sasname == rename else f"{sasname} ==> {rename}" for sasname, rename in list(zip(sel_cols, renames))]
                print(f"extrating {cyl}:  {file}  {variable_name_list}")
            df_concat.append(df_merge)
            print(f"merging {cyl}  files:  {merge_list}\n")
            concat_list.append(cyl)
        data3 = concat(df_concat, join='outer', axis=0)
        print(f"concating  {concat_list}\n")
        data3.index = range(len(data3.index))
        data3['SEQN'] = data3['SEQN'].apply(lambda x: int(x) if not isnan(x) else x)
        return data3

    def summary_PAQIAF(self, df):
        if self.findkey(['object'], str(df['PADLEVEL'].dtype)):
            df['PADLEVEL'] = df['PADLEVEL'].apply(
                lambda x: 1 if self.findkey(['MODERATE'], x) else 2 if self.findkey(['VIGOROUS'], x) else nan)
        df['pa_time'] = df['PADLEVEL'] * df['PADDURAT'] * df['PADTIMES']
        df_vigorous = df[df['PADLEVEL'] == 2]
        df_moderate = df[df['PADLEVEL'] == 1]

        vig_num = df_vigorous.groupby(by=['SEQN'])['PADTIMES'].sum().reset_index()
        mod_num = df_moderate.groupby(by=['SEQN'])['PADTIMES'].sum().reset_index()
        vig_num['PADTIMES'] = vig_num['PADTIMES'].apply(lambda x: nan if x == 0 else x)
        mod_num['PADTIMES'] = mod_num['PADTIMES'].apply(lambda x: nan if x == 0 else x)
        vig_num.columns = ['SEQN', 'vig_num_30_days']
        mod_num.columns = ['SEQN', 'mod_num_30_days']

        vig_time = df_vigorous.groupby(by=['SEQN'])['pa_time'].sum().reset_index()
        mod_time = df_moderate.groupby(by=['SEQN'])['pa_time'].sum().reset_index()
        vig_time['pa_time'] = vig_time['pa_time'].apply(lambda x: nan if x == 0 else x)
        mod_time['pa_time'] = mod_time['pa_time'].apply(lambda x: nan if x == 0 else x)
        vig_time.columns = ['SEQN', 'vig_total_30_days']
        mod_time.columns = ['SEQN', 'mod_total_30_days']

        df_time_total = df.groupby(by=['SEQN'])['pa_time'].sum().reset_index()
        df_time_total['pa_time'] = df_time_total['pa_time'].apply(lambda x: nan if x == 0 else x)
        df_time_total = df_time_total.loc[:, ['SEQN']]

        for inde, i in enumerate([df_time_total, vig_num, vig_time, mod_num, mod_time]):
            if inde == 0:
                df = i
            else:
                df = merge(df, i, how='outer', on=['SEQN'])
        return df
    def combine_files(self, df, codebook=False, summary_PAQIAF=False):
        df['path_to'] = self.dir + '/' + df['Component'] + '/' + df['Cycle']
        df['path_csv'] = df['path_to'].apply(lambda x: self.find_path_csv(x))
        df['path_codebook'] = df['path_to'].apply(lambda x: self.find_path_codebook(x))

        path_tos = df['path_to'].values
        path_csvs = df['path_csv'].values
        path_codebooks = df['path_codebook'].values

        path_csvs = [f"{a}/{b}" if a and b else '' for a, b in zip(path_tos, path_csvs) ]
        path_codebooks = [f"{a}/{b}" if a and b else '' for a, b in zip(path_tos, path_codebooks) ]
        file_names = df['File Name'].values

        csv_names = [self.find_csv_file(a, b) for a, b in zip(path_csvs, file_names)]
        codebook_names = [self.find_codebook_file(a, b) for a, b in zip(path_codebooks, file_names)]

        df['csv_name'] = csv_names
        df['codebook_name'] = codebook_names

        df['dst'] = df['path_to'] + '/' + df['path_csv'] + '/' + df['csv_name']
        if codebook:
            df['dst_cob'] = df['path_to'] + '/' + df['path_codebook'] + '/' + df['codebook_name']

        df['sas_name'] = df['Variable Name'].apply(lambda x: x.split(' ==> ')[0].upper())
        df['rename'] = df['Variable Name'].apply(lambda x: x.split(' ==> ')[1] if len(x.split(' ==> ')) > 1 else x.split(' ==> ')[0].upper())
        df_concat = []
        df_merge = None

        concat_list = []
        for cyl in df['Cycle'].unique():
            df_cyl = df[df['Cycle'] == cyl]
            df_cyl.index = range(len(df_cyl))
            # print(df_cyl)
            merge_list = []
            for inde, file in enumerate(df_cyl['File Name'].unique()):
                df_file = df_cyl[df_cyl['File Name'] == file]
                df_file.index = range(len(df_file))

                dst = df_file['dst'].values[0]
                # print(df_file['path_to'].values[0])
                # print(dst)
                # print(dst, inde)
                data = self.read_csv(dst)
                sel_cols = ['SEQN'] + list(df_file['sas_name'])
                renames = ['SEQN'] + list(df_file['rename'])
                try:
                    data2 = data.loc[:, sel_cols]
                except KeyError:
                    sel_cols = [sasname.replace('OSQ', 'OSD') if sasname.find('OSQ030') !=-1 else sasname for sasname in sel_cols]
                    data2 = data.loc[:, sel_cols]
                    print(f'{cyl}  OSQ030 replaced to OSD030 !')
                if codebook:
                    for i in df_file.index:
                        dst_cob = df_file.iloc[i, :]['dst_cob']
                        if dst_cob:
                            break
                    data2 = self.factor_data(data2, dst_cob)

                # if self.findkey(['PAQIAF.*'], file):
                if summary_PAQIAF and self.findkey(['PAQIAF.*'], file):
                    data2 = self.summary_PAQIAF(data2)
                else:
                    data2.columns = renames

                if inde==0:
                    df_merge = data2
                else:
                    df_merge = merge(df_merge, data2, how='outer', on=['SEQN'])
                merge_list.append(f'{file}')
                variable_name_list = [sasname  if sasname == rename else f"{sasname} ==> {rename}" for sasname, rename in list(zip(sel_cols, renames))]
                print(f"extrating {cyl}:  {file}  {variable_name_list}")
            df_concat.append(df_merge)
            print(f"merging {cyl}  files:  {merge_list}\n")
            concat_list.append(cyl)
        data3 = concat(df_concat, join='outer', axis=0)
        print(f"concating  {concat_list}\n")
        data3.index = range(len(data3.index))
        data3['SEQN'] = data3['SEQN'].apply(lambda x: int(x) if not isnan(x) else x)
        return data3
    def relevel(self, levels, x):
        value = x
        for level1, level2 in levels:
            if level1 == x:
                value = level2
                break
        return value

    def reinterval(self, intervals, x):
        value = x
        for interval, rename in intervals:
            if rename != '':
                left, right = findall("([\(|\[]* *[0-9.]+ *[\)\]]*)", interval)
                left, right = left.replace('(', ' >').replace('[', ' >='), right.replace(')', ' >').replace(']', ' >=')
                logstr = f"{x} {left} and {right} {x}"
                bool_res = eval(logstr)
                if bool_res:
                    value = rename
                    break
        return value

    def change_type(self, x, ):
        pass

    def percentile_level(self, breaks, df_var, mode, decimal):
        df_var = df_var.dropna()
        if type(breaks) == int:
            perc = 100/breaks
            breaks_list = [percentile(df_var, i*perc)  for i in range(1, breaks)]
            breaks_list = [df_var.min()] + breaks_list + [df_var.max()]
            breaks_list = [f'%.{decimal}f' % i for i in breaks_list]
            breaks_list = [(f"{mode[0]}{breaks_list[i-1]}, {breaks_list[i]}{mode[1]}", '') for i in range(1, len(breaks_list))]
        if type(breaks) == str:
            breaks2 = findall("[[0-9.]+", breaks)
            if breaks2:
                if len(breaks2) ==1 :
                    perc = 100/breaks
                    breaks_list = [ (f"{i*perc}%", percentile(i*perc))  for i in range(1, breaks)]
                else:
                    pass

        if type(breaks) == tuple or type(breaks) == list:

            if mode:
                breaks_list = [float(findall("[0-9.]+", f"{i}")[0]) for i in breaks]
                breaks_list = [df_var.min()] + breaks_list + [df_var.max()]
                breaks_list = [f'%.{decimal}f' % i for i in breaks_list]
                breaks_list = [ (f"{mode[0]}{breaks_list[i-1]}, {breaks_list[i]}{mode[1]}" ,'') for i in range(1, len(breaks_list))]
            else:
                breaks_list = [findall("([\(|\[]* *[0-9.]+ *[\)\]]*)", f"{i}") for i in breaks]
                breaks_list = [(left.replace(' ', ''), right.replace(' ', '')) for left, right in breaks_list]
                breaks_list = [(f"{left}, {right}", '') for left, right in breaks_list]
        return breaks_list

    def relevels_info(self, df, filter=None, lower=True, property={}):
        dic_levels = {}
        for inde, colname in enumerate(df.columns):

            col_type = str(df[colname].dtype)
            if self.findkey(['int', 'float'], col_type):
                levels = (df[colname].min(), df[colname].max())
                levels = [levels]

                properties = {'breaks':4, 'mode':'(]', 'intervals':[], 'decimal':1}
                if property:
                    for key in property.keys():
                        if key in properties.keys():
                            properties[key] = property[key]

                if properties['breaks']:
                    if type(properties['breaks']) == tuple or type(properties['breaks'])== list:
                        find_interval = [i for i in properties['breaks'] if findall("[\(|\)|\[|\]]", f"{i}")]
                        if find_interval:
                            properties['mode'] = ''
                            properties['decimal'] = ''

                    break_list = self.percentile_level(properties['breaks'], df[colname], properties['mode'], properties['decimal'])
                    properties['intervals'] = break_list

                    levels.append(properties)
            elif self.findkey(['object'], col_type):
                levels = df[colname].unique()
                levels = [ (level, '') for level in levels]
                if filter:
                    filter =self.deal_title_names(filter)
                    levels = [ (level1, level2) for level1, level2 in levels if self.findkey(filter, level1, lower=lower)]
            type_levels = [col_type] + levels
            dic_levels[colname] = type_levels
        printout = f"relevels={dic_levels}"
        for colname in df.columns:
            printout = printout.replace(f"'{colname}':", f"\n\n'{colname}':")
        printout = printout.replace("]}", "]\n      }         ").replace("}]", '}     ]')
        print(printout)
        copy(f", {printout}")



    def relevels_df(self, df , relevels):
        for key in relevels.keys():
                info = relevels[key]
                tp = info[0]

                if self.findkey(['object'], str(df[key].dtype)):
                    levels = info[1:]
                    if levels:
                        levels = [(a, b) for a, b in levels if b != '']
                        if levels:
                            df[key] = df[key].apply(lambda x: self.relevel(levels, x))
                elif self.findkey(['int', 'float'], str(df[key].dtype)):
                    dict_info = [i for i in info if type(i) == dict]
                    if dict_info:
                        dict_info = dict_info[0]
                        intervals = dict_info['intervals']
                        if intervals:
                            df[key] = df[key].apply(lambda x: self.reinterval(intervals, x))

                if tp != str(df[key].dtype):
                    df[key] = df[key].astype(tp)
        return df

    def match_level(self, x, level):
        if f"{x}" == f"{level}":
            return True
        else:
            return False

    def levels(self, df):
        infos = []
        for col in df.columns:

              tp = str(df[col].dtype)
              if self.findkey(['object'], tp):
                    levels = list(df[col].unique())
                    lengths = [ len(df[df[col].apply(lambda x: self.match_level(x, level))]) for level in levels]
                    info = list(zip(levels, lengths))
                    info = [col] + info
                    infos.append(info)

        return infos
    def sys_mean(self, row_seriers):

        row = row_seriers[['BPXSY1', 'BPXSY2', 'BPXSY3', 'BPXSY4']]
        values = list(row.values)
        values = [ i for i in values if not isnan(i)]
        num = len(values) if values else nan

        if isnan(num):
            values_avg = nan
        else:
            values_ef = values[1:] if num > 1 else values
            values_avg = sum(values_ef)/len(values_ef)

        return values_avg
    def dia_mean(self, row_seriers):
        row = row_seriers[['BPXDI1', 'BPXDI2', 'BPXDI3', 'BPXDI4']]
        values = list(row.values)
        values = [ i for i in values if not isnan(i)]
        num = len(values) if values else nan

        values_avg = nan
        if isnan(num):
              values_avg = nan
        else:
              find_0 = [inde for inde, i in enumerate(values) if i == 0]
              if find_0:
                    # print(find_0)
                    if len(find_0) == 3:
                          values_avg = 0
                    elif find_0[0] == 0:
                          # print(f"step 1")
                          values_ef =  [i for i in values if i != 0]
                          values_ef = values_ef if values_ef else [0]
                          values_avg = sum(values_ef)/len(values_ef)
                    else:
                          # print(f"step 2")
                          values_ef =  [i for i in values if i != 0]
                          values_ef = values_ef if values_ef else [0]
                          values_ef = values_ef[1:] if len(values_ef) > 1 else values_ef
                          values_avg = sum(values_ef)/len(values_ef)
              else:
                    # print(f"step 3")
                    values_ef = values[1:] if num > 1 else values
                    values_avg = sum(values_ef)/len(values_ef)
        return values_avg

    def sum_PHQ9(self, df):
        cols = [f"DPQ0{i}0" for i in range(1, 10)]
        df_PHQ9 = df.loc[:, cols]
        PHQ9 = df_PHQ9.apply(lambda row: sum(row), axis=1)
        return PHQ9

    def left_join(self, df_list, by=['SEQN'], by_how='&'):
        length_by = len(by) if type(by) == list or type(by) == tuple else 1
        if (type(by) == list or type(by) == tuple):
            if len(by) > 1:
                by = by
            elif len(by) == 1:
                by = ''.join(by)

        if type(by) == str:
            by = by

        df = df_list[0]
        df_list = df_list[1:]

        if df_list:
            for inde, df_i in enumerate(df_list):

                if (type(by) == list or type(by) == tuple):
                    log_col = df_i[by].isin(df[by]).sum(axis=1)
                    if by_how == '&':
                        log_col = log_col.apply(lambda x: True if x==length_by else False)
                    elif by_how == '|':
                        log_col = log_col.apply(lambda x: True if x>0 else False)

                if type(by) == str:
                    log_col = df_i[by].isin(df[by])


                df_i = df_i[log_col]

                if df_i.empty:
                    print(f"df {inde + 1}--> common cols [{by}]:  没有 Left 交集！")
                else:
                    print(f"df {inde + 1}--> common cols [{by}]:  RangeSEQN({df_i['SEQN'].min()}, {df_i['SEQN'].max()}) LengthSEQN={len(df_i['SEQN'])}")
                    df = merge(df, df_i, how='outer')


        return df


    def get_drug(self, df, drug_name=['tramadol'], mode='|'):
        if list(df['take_pre_past_month'])[0] == 'Yes':

            find_pre = [i for i in list(df['pre_name']) if self.findkey(drug_name, i, mode=mode)]
            find_pre = [i for i in find_pre if i]
            # print(find_pre)
            if find_pre:
                drugs_find = '--'.join(find_pre) if len(find_pre) > 1 else ''.join(find_pre)

            else:
                drugs_find = 'No'
            # yes_or_no = 'Yes'
        elif list(df['take_pre_past_month'])[0] == 'No':
            drugs_find = 'No'
            # yes_or_no = 'No'
        else:
            # yes_or_no = nan
            drugs_find = nan

        return drugs_find


    def get_drug_his(self, df_pre, drug_name, colname=None ,mode='|'):
        bindstr = '_or_' if mode == '|' else '_and_' if mode == '&' else ' '
        if colname is None:
            colname = f'{bindstr}'.join(drug_name) if len(drug_name) > 1 else ''.join(drug_name)
        df_pre = df_pre.groupby('SEQN')
        df5 = df_pre.apply(lambda df: self.get_drug(df, drug_name, mode=mode))
        df5 = DataFrame({f'{colname}': df5})
        df5['SEQN'] = df5.index
        df5.index = range(len(df5.index))
        df5 = df5.loc[:, ['SEQN', colname]]
        return df5



    def join_df(self, df_list, by=['SEQN'], how='inner', ref=0):

        df_ref = df_list[ref]
        df_list = [df for inde, df in enumerate(df_list) if inde!=ref]
        df_merge = df_ref
        for inde, df in enumerate(df_list):
            df_merge = merge(df_merge, df, how='outer', on=by)

        if how == 'inner':
            df_merge = merge(df_ref.loc[:, by], df_merge, on= by, how='inner')
        return df_merge









if __name__ == '__main__':
    nhanes = nhanes_v()